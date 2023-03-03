(ns datalog.analysis
  (:require [clojure.set :as set]
            [datalog.analysis.graph :as ag]
            [datalog.analysis.mode :as am]
            [datalog.parser.impl :as impl]
            [datalog.parser.impl.util :as util]
            [datalog.parser.type :as t]
            [medley.core :as m]))

;; cycles in rules

(defn branch-calls-rule [rule-name branch]
  (->> (:clauses branch)
       (tree-seq seq? identity)
       (m/find-first #(and (symbol? %2)
                           (= (name %2) rule-name)))))

(defn rule-calls-rule [rule1 rule-name2]
  (m/find-first (partial branch-calls-rule rule-name2)
                (:branches rule1)))

(defn recursive-rules
  "Returns names of rules with cycles"
  [rules]
  (let [dependencies (->> rules
                          (map #(vector (:name %)
                                        (->> (keys rules)
                                             (filter (partial rule-calls-rule %))
                                             set)))
                          (apply hash-map))]

    (filter #(ag/has-cycle? dependencies %)
            (keys dependencies))))

;; range restriction

(defn check-range-restriction
  
  [parsed-rules]
  (run! (fn [[rule-name branches]]
          (for [{:keys [vars clauses]} branches
                :let [missing (set/difference
                               (set (impl/flatten-rule-vars vars))
                               (t/collect-vars #{} clauses))]
                :when (seq missing)]
            (util/raise "Rule body must contain all vars from rule head"
                        {:error :analyzer/range-restriction, :rule rule-name :clauses clauses :missing missing})))
        parsed-rules))


;; modes

(defn well-modable
  "Detemines moding options for clauses in a bottom.up fashion"
  ([parsed-query] (well-modable parsed-query {}))
  ([{:keys [qwhere qin]} parsed-rules]
   (let [in-vars qin
         rule-modes (am/infere-rule-modes parsed-rules)
         where-modes (am/infere-where-modes qwhere rule-modes)
         optional-obligations (:obligations (am/obligation-map where-modes))]
     (when-not (m/find-first #(set/subset? % in-vars) optional-obligations)
       (util/raise (str "Query is not satisfiable. "
                        "Possible obligations are: " optional-obligations)))
     where-modes)))

(defn well-moded
  "Simulates the sequential execution of clauses to determine well-modedness.
   Returns sets of eventual bindings."
  ([parsed-query] (well-moded parsed-query {}))
  ([{:keys [qwhere qin]} parsed-rules]
   (let [in-vars qin
         rule-modes (am/infere-rule-modes parsed-rules)]
     (am/sequential-bindings rule-modes in-vars qwhere))))
