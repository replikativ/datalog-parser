(ns datalog.analysis.mode
  (:require [clojure.math.combinatorics :as c]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [datalog.analysis.proto :as ap]
            [datalog.parser.impl :as impl]
            [datalog.parser.impl.proto :as p]
            [datalog.parser.impl.util :as util]
            [datalog.parser.type :as t]
            [medley.core :as m]
            [spec-tools.data-spec :as ds]))

(defn set-of [pred] (s/every pred :kind set?))
(defn vec-of [pred] (s/every pred :kind vector?))

(def SModedClause
  (ds/spec
   {:name ::moded-clause
    :keys-default ds/req
    :spec {:form (partial satisfies? p/ITraversable)
           :vars (vec-of symbol?)
           :modes (set-of (set-of :symbol))}})) ;; TODO: probably set of symbols is enough

;; well modable

(defn min-modes
  "Apply min to mode-set"
  [modes]
  (reduce (fn [mm new-mode] (if-let [supersets (seq (filter (partial set/subset? new-mode) mm))]
                              (conj (set/difference mm supersets) new-mode)
                              (if (seq (filter (partial set/superset? new-mode) mm))
                                mm
                                (conj mm new-mode))))
          #{(first modes)}
          (rest modes)))

(defn translate-modes
  "Returns translations of a mode set.
   Throws if a mode set is invalid in this context.
   An empty mode set indicates an unsatisfiable clause in the current context."
  [new-vars {:keys [modes vars] :as moded-clause}]
  (let [index-translation (->> vars
                               (map-indexed (fn [idx old-var] [idx (.indexOf new-vars old-var)]))
                               (apply hash-map)) 
        new-modes (map #(replace index-translation %) modes)
        {valid false
         invalid true} (group-by #(contains? % -1) new-modes)]
  (when (seq invalid)
    (util/raise "Invalid modes in this context: " invalid
                {:clause moded-clause
                 :new-vars new-vars
                 :new-modes new-modes}))
    (set valid)))

(defn join-domains
  "Join mode domains of different clauses"
  ([moded-clauses] (join-domains moded-clauses nil))
  ([moded-clauses bindable-vars]
   (let [all-vars (->> moded-clauses (map :vars) set)
         new-vars (vec (if bindable-vars
                         (set/intersection all-vars bindable-vars)
                         all-vars))
         new-modes (map (partial translate-modes new-vars)
                        moded-clauses)]
     {:vars new-vars
      :mode-sets new-modes})))

(defn merge-clauses
  "Join parallel clauses"
  [moded-clauses]
  (let [{:keys [vars mode-sets]} (join-domains moded-clauses)]
    {:form (t/->AndP (map :clause moded-clauses))
     :vars vars
     :modes (->> mode-sets
                 (reduce (fn [acc-modes new-modes]
                           (mapcat (fn [modes1]
                                     (map (fn [modes2] (set/union modes1 modes2))
                                          new-modes))
                                   acc-modes))
                         [])
                 set
                 min-modes)}))

(defn join-or-clauses
  "Join alternative clauses"
  [orig bindable-vars moded-clauses]
  (let [{:keys [vars mode-sets]} (join-domains bindable-vars moded-clauses)
        valid (->> moded-clauses
                   (map #(hash-map :modes %1 :clause %2) mode-sets)
                   (filter #(seq (:modes %))))]
    (when (empty? valid)
      (util/raise "No valid alternatives in Or-branch for this environment"
                  {:vars bindable-vars
                   :clauses moded-clauses}))
    {:form orig
     :vars vars
     :modes (->> (map :modes valid)
                 (apply concat)
                 set
                 min-modes)
     :alts (map :clause valid)}))

(defn remove-bound [obligations newly-bound]
  (->> obligations
       (map #(set/difference % newly-bound))
       (filter seq)
       set))

(defn greedy-obligation-paths
  "Calculates minimal orderings of clauses of a single level.
   Returns nil on failure.
   Returns sequences of clauses with last element containing the obligation set."
  [bindable-vars bound-vars obligation-acc edges]
  (let [{natural true
         non-natural false} (group-by #(contains? (:obligations %) #{})
                                      edges)
        second-choices (filter (fn [edge] (seq (m/find-first (fn [obligation] (set/subset? obligation bindable-vars))
                                                             (:obligations edge))))
                               non-natural)]
    (cond
      (seq natural)           ;; natural edges with empty / trivial obligation
      (let [joined (merge-clauses (map :origin natural))
            newly-bound (set/difference (:vars joined) bound-vars)]
        (->> non-natural
             (map #(update % :obligations remove-bound newly-bound))
             (greedy-obligation-paths bindable-vars
                                      (set/union bound-vars newly-bound)
                                      obligation-acc)
             (filter identity)
             (map #(cons joined %))))

      (empty? non-natural)    ;; terminal vertex, successful
      [[{:obligation obligation-acc}]]

      (empty? second-choices) ;; unsatisfiable path
      nil

      :else                   ;; picks from non-natural edges with satifiable obligation
      (->> second-choices
           (map (fn [{:keys [obligations var-set origin] :as picked}]
                  (let [newly-bound (set/difference var-set bound-vars)
                        updated-edges (->> non-natural
                                           (filter #(= picked %))
                                           (map #(update % :obligations remove-bound newly-bound)))]
                    (->> obligations
                         (mapcat (fn [obligation]
                                   (->> updated-edges
                                        (greedy-obligation-paths bindable-vars
                                                                 (set/union bound-vars newly-bound)
                                                                 (conj obligation-acc obligation))
                                        (filter identity)
                                        (map #(cons origin %)))))))))))))

(defn obligation-map [{:keys [modes vars] :as moded-clause}]
  {:obligations (walk/postwalk-replace vars modes)
   :var-set (set vars)
   :origin moded-clause})

(defn min-reorder
  "Finds min-reorderings and modes for clauses in a common domain"
  [orig bindable-vars moded-clauses]
  (let [paths (->> moded-clauses
                   (map obligation-map)
                   (greedy-obligation-paths bindable-vars #{} #{}))
        _ (when-not paths
            (util/raise "No satisfiable reordering for clause set"
                        {:clauses (mapv :clause moded-clauses)
                         :bindable-vars bindable-vars}))
        vars (vec bindable-vars)
        obl->mode (set/map-invert vars)
        alts (map (fn [path] {:form (vec (butlast path))
                              :modes #{(replace obl->mode (:obligation (last path)))}})
                  paths)]
    {:form orig
     :vars vars
     :modes (set (map (comp first :modes) alts))
     :alts alts})) ;; TODO: probably no alts needed as every alt needs to be  applicable

(declare infere-and-context-modes infere-or-context-modes)

(defn infere-clause-mode [bindable-vars rule-modes clause]
  (cond
    (satisfies? ap/IInitialModing clause) (ap/-initial-modes clause)
    (instance? t/RuleExpr clause)      (rule-modes (:name clause))
    (instance? t/Or clause)            (infere-or-context-modes clause (:rule-vars clause) rule-modes (:clauses clause))
    (instance? t/And clause)           (infere-and-context-modes clause bindable-vars rule-modes (:clauses clause))
    (instance? t/Not clause)           (infere-and-context-modes clause (:vars clause) rule-modes (:clauses clause))))


(defn infere-or-context-modes ;; TODO: change to apply + operation instead of min as all brnaches need to be able to run
  "Find modes of all clauses and infere min modes of branches"
  [orig bindable-vars rule-modes moded-clauses]
  (->> moded-clauses
       (map #(infere-clause-mode bindable-vars rule-modes %))
       (join-or-clauses orig bindable-vars)))

(defn infere-and-context-modes
  "Find modes of all clauses and infere min modes of context by reordering"
  [orig bindable-vars rule-modes moded-clauses]
  (->> moded-clauses
       (map #(infere-clause-mode bindable-vars rule-modes %))
       (partial min-reorder orig bindable-vars)))

(defn rule-vars [rule]
  (-> rule :branches first :vars impl/flatten-rule-vars vec))

(defn infere-rule-modes [rules]
  (let [rule-map (m/indexed-map rules :name)]
    (loop [modes (m/map-vals #(set (set (range (count (rule-vars %)))))
                             rules)]
      (let [new-modes (m/map-vals (fn [{:keys [branches] :as rule}]
                                    (let [vars (set (rule-vars rule))]
                                      (->> branches
                                           (map #(infere-and-context-modes % vars modes (:clauses %)))
                                           (infere-or-context-modes rule vars modes))))
                                  rule-map)]
        (if (= modes new-modes)
          modes
          (recur new-modes))))))

(defn infere-where-modes [qwhere rule-modes]
  (let [vars (t/collect-vars #{} qwhere)]
    {:form qwhere
     :vars vars
     :modes (infere-and-context-modes (t/And qwhere) vars rule-modes qwhere)}))

;; well moded

(defn join-binding-options [options1 options2]
  (->> (c/cartesian-product options1 options2)
       (map (partial apply set/union))
       (reduce into #{})))

(defn valid-bindings [binding-options required]
  (->> binding-options
       (map #(set/difference required %))
       (remove empty?)))

(defn resolve-rule-options ;; return binding options
  "Returns vars that can be bound by any of the options"
  [binding-options rule-mode clause]
  (let [moded-clause (assoc rule-mode :vars (rule-vars clause))
        {:keys [var-set obligations]} (obligation-map moded-clause)
        options (map #(let [valid-options (valid-bindings binding-options (:required %))]
                        (hash-map :required %
                                  :valid-options valid-options
                                  :out (set/difference var-set %)))
                     obligations)
        valid (filter (comp seq :valid-options) options)]
    (if (empty? valid)
      (util/raise "No satisfiable mode found for '" clause
                  {:binding-options binding-options
                   :required-options (set (map :required options))})
      (->> valid
           (map #(join-binding-options (:valid-options %)
                                       [(set/difference var-set (:required %))]))
           set))))

(defn resolve-options [binding-options required-vars bindings clause]
  (let [valid-options (valid-bindings binding-options required-vars)]
    (if (empty? valid-options)
      (util/raise "No satisfiable mode found for '" clause
                  {:binding-options binding-options
                   :required required-vars})
      (join-binding-options (set valid-options) [bindings]))))

(declare sequential-bindings)

(defn resolve-clause [rule-modes binding-options clause]
  (cond
    (satisfies? ap/IInitialModing clause) (resolve-options binding-options (ap/-required clause) (ap/-binds clause) clause)
    (instance? t/RuleExpr clause)      (resolve-rule-options binding-options (rule-modes (:name clause)) clause)
    (instance? t/Or clause)            (set (mapcat (partial resolve-clause rule-modes binding-options)
                                                    (:clauses clause)))
    (instance? t/And clause)           (sequential-bindings rule-modes binding-options (:clauses clause))
    (instance? t/Not clause)           (do (sequential-bindings rule-modes #{} (:clauses clause))
                                           (resolve-options binding-options (:vars clause) #{} clause))))

(defn sequential-bindings
  "Computes the sets of bindings returned by executing the queries sequentially"
  [rule-modes binding-options clauses]
  (reduce (partial resolve-clause rule-modes) binding-options clauses))
