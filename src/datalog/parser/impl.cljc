(ns datalog.parser.impl
  (:require [clojure.set               :as set]
            [datalog.parser.type       :as t]
            [datalog.parser.util                :refer [postwalk]]
            [datalog.parser.impl.proto :as p]
            [datalog.parser.impl.util :as util
              #?(:cljs :refer-macros :clj :refer) [raise forv]]
            #?(:cljs [datalog.parser.type :refer
                      [Not And Or Aggregate SrcVar RulesVar RuleExpr
                       RuleVars Variable ReturnMaps MappingKey]]))
  (:refer-clojure :rename  {distinct? core-distinct?})
  #?(:clj
     (:import [datalog.parser.type
               Not And Or Aggregate SrcVar RulesVar RuleExpr RuleVars Variable ReturnMaps MappingKey])))

#?(:clj (set! *warn-on-reflection* true))

(declare parse-clause parse-clauses parse-binding)

(defn of-size? [form size]
  (and (sequential? form) (= (count form) size)))

(defn parse-seq [parse-el form]
  (when (sequential? form)
    (reduce
     (fn item-parser [acc el]
       (if-let [parsed (parse-el el)]
         (conj acc parsed)
         (reduced nil)))
     [] form)))

(defn collect-type
  ([t form]     (collect-type t form []))
  ([t form acc] (util/collect #(instance? t %) form acc)))

(defn- distinct? [coll]
  (or (empty? coll) (apply core-distinct? coll)))

(defn with-source [obj source]
  (vary-meta obj assoc :source source))

;; `source` conflicts w/ clojure.repl
(defn get-source [obj]
  (or (:source (meta obj)) obj))

(defn parse-placeholder [form]
  (when (= '_ form)
    (datalog.parser.type.Placeholder.)))

(defn parse-variable [form]
  (when (and (symbol? form) (= (first (name form)) \?))
    (Variable. form)))

(defn parse-src-var [form]
  (when (and (symbol? form) (= (first (name form)) \$))
    (SrcVar. form)))

(defn parse-rules-var [form]
  (when (= '% form)
    (RulesVar.)))

(defn parse-constant [form]
  (when (not (symbol? form))
    (datalog.parser.type.Constant. form)))

(defn parse-plain-symbol [form]
  (when (and (symbol? form)
             (not (parse-variable    form))
             (not (parse-src-var     form))
             (not (parse-rules-var   form))
             (not (parse-placeholder form)))
    (datalog.parser.type.PlainSymbol. form)))

(defn parse-plain-variable [form]
  (when (parse-plain-symbol form)
    (Variable. form)))

;; fn-arg = (variable | constant | src-var)

(defn parse-fn-arg [form]
  (or (parse-variable form)
      (parse-constant form)
      (parse-src-var  form)))

;; rule-vars = [ variable+ | ([ variable+ ] variable*) ]

(defn parse-rule-vars [form]
  (if (sequential? form)
    (let [[required rest] (if (sequential? (first form))
                            [(first form) (next form)]
                            [nil form])
          required* (parse-seq parse-variable required)
          free*     (parse-seq parse-variable rest)]
      (when (and (empty? required*) (empty? free*))
        (raise "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
               {:error :parser/rule-vars, :form form}))
      (when-not (distinct? (concat required* free*))
        (raise "Rule variables should be distinct"
               {:error :parser/rule-vars, :form form}))
      (RuleVars. required* free*))
    (raise "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
           {:error :parser/rule-vars, :form form})))

(defn flatten-rule-vars [rule-vars]
  (concat
   (when (:required rule-vars)
     [(mapv :symbol (:required rule-vars))])
   (map :symbol (:free rule-vars))))

(defn rule-vars-arity [rule-vars]
  [(count (:required rule-vars)) (count (:free rule-vars))])

(defn parse-bind-ignore [form]
  (when (= '_ form)
    (with-source (datalog.parser.type.BindIgnore.) form)))

(defn parse-bind-scalar [form]
  (when-let [var (parse-variable form)]
    (with-source (datalog.parser.type.BindScalar. var) form)))

(defn parse-bind-coll [form]
  (when (and (of-size? form 2) (= (second form) '...))
    (if-let [sub-bind (parse-binding (first form))]
      (-> (datalog.parser.type.BindColl. sub-bind)
          (with-source form))
      (raise "Cannot parse collection binding"
             {:error :parser/binding, :form form}))))

(defn parse-tuple-el [form]
  (or (parse-bind-ignore form)
      (parse-binding form)))

(defn parse-bind-tuple [form]
  (when-let [sub-bindings (parse-seq parse-tuple-el form)]
    (if-not (empty? sub-bindings)
      (-> (datalog.parser.type.BindTuple. sub-bindings)
          (with-source form))
      (raise "Tuple binding cannot be empty"
             {:error :parser/binding, :form form}))))

(defn parse-bind-rel [form]
  (when (and (of-size? form 1) (sequential? (first form)))
    ;; relation is just a sequence of tuples
    (-> (parse-bind-tuple (first form))
        datalog.parser.type.BindColl.
        (with-source form))))

(defn parse-binding [form]
  (or (parse-bind-coll form)
      (parse-bind-rel form)
      (parse-bind-tuple form)
      (parse-bind-ignore form)
      (parse-bind-scalar form)
      (raise "Cannot parse binding, expected (bind-scalar | bind-tuple | bind-coll | bind-rel)"
             {:error :parser/binding, :form form})))

(defn find-vars [find]
  (mapcat p/-find-vars (p/find-elements find)))

(defn parse-aggregate [form]
  (when (and (sequential? form) (>= (count form) 2))
    (let [[fn & args] form
          fn*   (parse-plain-symbol fn)
          args* (parse-seq parse-fn-arg args)]
      (when (and fn* args*)
        (Aggregate. fn* args*)))))

(defn parse-aggregate-custom [form]
  (when (and (sequential? form) (= (first form) 'aggregate))
    (if (>= (count form) 3)
      (let [[_ fn & args] form
            fn*   (parse-variable fn)
            args* (parse-seq parse-fn-arg args)]
        (if (and fn* args*)
          (Aggregate. fn* args*)
          (raise "Cannot parse custom aggregate call, expect ['aggregate' variable fn-arg+]"
                 {:error :parser/find, :fragment form})))
      (raise "Cannot parse custom aggregate call, expect ['aggregate' variable fn-arg+]"
             {:error :parser/find, :fragment form}))))

(defn parse-pull-expr [form]
  (when (and (sequential? form) (= (first form) 'pull))
    (if (<= 3 (count form) 4)
      (let [long?         (= (count form) 4)
            src           (if long? (nth form 1) '$)
            [var pattern] (if long? (nnext form) (next form))
            src*          (parse-src-var src)
            var*          (parse-variable var)
            pattern*      (or (parse-variable pattern)
                              (parse-plain-variable pattern)
                              (parse-constant pattern))]
        (if (and src* var* pattern*)
          (datalog.parser.type.Pull. src* var* pattern*)
          (raise "Cannot parse pull expression, expect ['pull' src-var? variable (constant | variable | plain-symbol)]"
                 {:error :parser/find, :fragment form})))
      (raise "Cannot parse pull expression, expect ['pull' src-var? variable (constant | variable | plain-symbol)]"
             {:error :parser/find, :fragment form}))))

(defn parse-find-elem [form]
  (or (parse-variable form)
      (parse-pull-expr form)
      (parse-aggregate-custom form)
      (parse-aggregate form)))

(defn parse-find-rel [form]
  (some->
   (parse-seq parse-find-elem form)
   (datalog.parser.type.FindRel.)))

(defn parse-find-coll [form]
  (when (and (sequential? form) (= (count form) 1))
    (let [inner (first form)]
      (when (and (sequential? inner)
                 (= (count inner) 2)
                 (= (second inner) '...))
        (some-> (parse-find-elem (first inner))
                (datalog.parser.type.FindColl.))))))

(defn parse-find-scalar [form]
  (when (and (sequential? form) (= (count form) 2) (= (second form) '.))
    (some-> (parse-find-elem (first form))
            (datalog.parser.type.FindScalar.))))

(defn parse-find-tuple [form]
  (when (and (sequential? form) (= (count form) 1))
    (let [inner (first form)]
      (some->
       (parse-seq parse-find-elem inner)
       (datalog.parser.type.FindTuple.)))))

(defn parse-find [form]
  (or (parse-find-rel    form)
      (parse-find-coll   form)
      (parse-find-scalar form)
      (parse-find-tuple  form)
      (raise "Cannot parse :find, expected: (find-rel | find-coll | find-tuple | find-scalar)"
             {:error :parser/find, :fragment form})))

;; with = [ variable+ ]

(defn parse-with [form]
  (or (parse-seq parse-variable form)
      (raise "Cannot parse :with clause, expected [ variable+ ]"
             {:error :parser/with, :form form})))

;; in = [ (src-var | rules-var | plain-symbol | binding)+ ]

(defn- parse-in-binding [form]
  (if-let [var (or (parse-src-var        form)
                   (parse-rules-var      form)
                   (parse-plain-variable form))]
    (with-source (datalog.parser.type.BindScalar. var) form)
    (parse-binding form)))

(defn parse-in [form]
  (or (parse-seq parse-in-binding form)
      (raise "Cannot parse :in clause, expected (src-var | % | plain-symbol | bind-scalar | bind-tuple | bind-coll | bind-rel)"
             {:error :parser/in, :form form})))

(defn parse-pattern-el [form]
  (or (parse-placeholder form)
      (parse-variable    form)
      (parse-constant    form)))

(defn take-source [form]
  (when (sequential? form)
    (if-let [source* (parse-src-var (first form))]
      [source* (next form)]
      [(datalog.parser.type.DefaultSrc.) form])))

(defn parse-pattern [form]
  (when-let [[source* next-form] (take-source form)]
    (when-let [pattern* (parse-seq parse-pattern-el next-form)]
      (if-not (empty? pattern*)
        (-> (datalog.parser.type.Pattern. source* pattern*)
            (with-source form))
        (raise "Pattern could not be empty"
               {:error :parser/where, :form form})))))

(defn parse-call [form]
  (when (sequential? form)
    (let [[fn & args] form
          args        (if (nil? args) [] args)
          fn*         (or (parse-plain-symbol fn)
                          (parse-variable     fn))
          args*       (parse-seq parse-fn-arg args)]
      (when (and fn* args*)
        [fn* args*]))))

(defn parse-pred [form]
  (when (of-size? form 1)
    (when-let [[fn* args*] (parse-call (first form))]
      (-> (datalog.parser.type.Predicate. fn* args*)
          (with-source form)))))

(defn parse-fn [form]
  (when (of-size? form 2)
    (let [[call binding] form]
      (when-let [[fn* args*] (parse-call call)]
        (when-let [binding* (parse-binding binding)]
          (-> (datalog.parser.type.Function. fn* args* binding*)
              (with-source form)))))))

(defn parse-rule-expr [form]
  (when-let [[source* next-form] (take-source form)]
    (let [[name & args] next-form
          name*         (parse-plain-symbol name)
          args*         (parse-seq parse-pattern-el args)]
      (when name*
        (cond
          (empty? args)  (raise "rule-expr requires at least one argument"
                                {:error :parser/where, :form form})
          (nil?   args*) (raise (str "Cannot parse rule-expr arguments, expected"
                                     " [ (variable | constant | '_')+ ]")
                                {:error :parser/where, :form form})
          :else          (RuleExpr. source* name* args*))))))

(defn collect-vars-distinct [form]
  (into [] (distinct (t/collect-vars form))))

(defn- validate-join-vars [vars clauses form]
  (let [undeclared (set/difference (set vars)
                                   (t/collect-vars #{} clauses))]
    (when-not (empty? undeclared)
      (raise "Join variable not declared inside clauses: " (mapv :symbol undeclared)
             {:error :parser/where :form form})))
  (when (empty? vars)
    (raise "Join variables should not be empty"
           {:error :parser/where :form form})))

(defn- validate-or-join-vars [vars clauses form]
  (when (empty? vars)
    (raise "Join variables should not be empty"
           {:error :parser/where :form form})))

(defn- validate-not [clause form]
  (validate-join-vars (:vars clause) (:clauses clause) form)
  clause)

(defn parse-not [form]
  (when-let [[source* next-form] (take-source form)]
    (let [[sym & clauses] next-form]
      (when (= 'not sym)
        (if-let [clauses* (parse-clauses clauses)]
          (-> (Not. source* (collect-vars-distinct clauses*) clauses*)
              (with-source  form)
              (validate-not form))
          (raise "Cannot parse 'not' clause, expected [ src-var? 'not' clause+ ]"
                 {:error :parser/where, :form form}))))))

(defn parse-not-join [form]
  (when-let [[source* next-form] (take-source form)]
    (let [[sym vars & clauses] next-form]
      (when (= 'not-join sym)
        (let [vars*    (parse-seq parse-variable vars)
              clauses* (parse-clauses clauses)]
          (if (and vars* clauses*)
            (-> (Not. source* vars* clauses*)
                (with-source  form)
                (validate-not form))
            (raise "Cannot parse 'not-join' clause, expected [ src-var? 'not-join' [variable+] clause+ ]"
                   {:error :parser/where, :form form})))))))

(defn validate-or [clause form]
  (let [{{required :required
          free     :free} :rule-vars} clause
        vars                          (concat required free)]
    (doseq [clause (:clauses clause)]
      (validate-join-vars vars [clause] form))
    clause))

(defn validate-or-join [clause form]
  (let [{{required :required
          free     :free} :rule-vars} clause
        vars                          (concat required free)]
    (doseq [clause (:clauses clause)]
      (validate-or-join-vars vars [clause] form))
    clause))

(defn parse-and [form]
  (when (and (sequential? form) (= 'and (first form)))
    (let [clauses* (parse-clauses (next form))]
      (if (not-empty clauses*)
        (And. clauses*)
        (raise "Cannot parse 'and' clause, expected [ 'and' clause+ ]"
               {:error :parser/where, :form form})))))

(defn parse-or [form]
  (when-let [[source* next-form] (take-source form)]
    (let [[sym & clauses] next-form]
      (when (= 'or sym)
        (if-let [clauses* (parse-seq (some-fn parse-and parse-clause) clauses)]
          (-> (Or. source* (RuleVars. nil (collect-vars-distinct clauses*)) clauses*)
              (with-source form)
              (validate-or form))
          (raise "Cannot parse 'or' clause, expected [ src-var? 'or' clause+ ]"
                 {:error :parser/where, :form form}))))))

(defn parse-or-join [form]
  (when-let [[source* next-form] (take-source form)]
    (let [[sym vars & clauses] next-form]
      (when (= 'or-join sym)
        (let [vars*    (parse-rule-vars vars)
              clauses* (parse-seq (some-fn parse-and parse-clause) clauses)]
          (if (and vars* clauses*)
            (-> (Or. source* vars* clauses*)
                (with-source form)
                (validate-or-join form))
            (raise "Cannot parse 'or-join' clause, expected [ src-var? 'or-join' [variable+] clause+ ]"
                   {:error :parser/where, :form form})))))))

(defn parse-clause [form]
  (or (parse-not       form)
      (parse-not-join  form)
      (parse-or        form)
      (parse-or-join   form)
      (parse-pred      form)
      (parse-fn        form)
      (parse-rule-expr form)
      (parse-pattern   form)
      (raise (str "Cannot parse clause, expected (data-pattern | pred-expr |"
                  " fn-expr | rule-expr | not-clause | not-join-clause |"
                  " or-clause | or-join-clause)")
             {:error :parser/where, :form form})))

(defn parse-clauses [clauses]
  (parse-seq parse-clause clauses))

(defn parse-limit
  "Parse pagination limit"
  [limit]
  (when limit
    (if (= (type (first limit)) java.lang.Long)
      (first limit)
      (raise "Cannot parse :limit, expected java.lang.Long"
             {:error :parser/limit, :limit limit}))))

(defn parse-offset
  "Parse pagination offset"
  [offset]
  (when offset
    (if (= (type (first offset)) java.lang.Long)
      (first offset)
      (raise "Cannot parse :offset, expected java.lang.Long"
             {:error :parser/offset, :offset offset}))))

(defn parse-return-maps
  "Parse request to return maps"
  [return-maps]
  (let [type    (first (keys return-maps))
        toomany (> (count (keys return-maps)) 1)]
    (if toomany
      (raise "Only one of these three options is allowed: :keys :strs :syms"
             {:error :parser/return-maps, :return-maps return-maps})
      (when type
        (ReturnMaps.
         type
         (map #(MappingKey. %) (get return-maps type)))))))

(defn parse-where [form]
  (or (parse-clauses form)
      (raise "Cannot parse :where clause, expected [clause+]"
             {:error :parser/where, :form form})))

(defn validate-vars [vars clauses form]
  (let [declared-vars   (collect-type Variable vars    #{})
        used-vars       (collect-type Variable clauses #{})
        undeclared-vars (set/difference used-vars declared-vars)]
    (when-not (empty? undeclared-vars)
      (raise "Reference to the unknown variables: " (map :symbol undeclared-vars)
             {:error :parser/rule, :form form, :vars undeclared-vars}))))

(defn parse-rule [form]
  (if (sequential? form)
    (let [[head & clauses] form]
      (if (sequential? head)
        (let [[name & vars] head
              name*    (or (parse-plain-symbol name)
                           (raise "Cannot parse rule name, expected plain-symbol"
                                  {:error :parser/rule, :form form}))
              vars*    (parse-rule-vars vars)
              clauses* (or (not-empty (parse-clauses clauses))
                           (raise "Rule branch should have clauses"
                                  {:error :parser/rule, :form form}))]
          (validate-vars vars* clauses* form)
          {:name    name*
           :vars    vars*
           :clauses clauses*})
        (raise "Cannot parse rule head, expected [rule-name rule-vars]"
               {:error :parser/rule, :form form})))
    (raise "Cannot parse rule, expected [rule-head clause+]"
           {:error :parser/rule, :form form})))

(defn validate-arity [name branches]
  (let [vars0  (:vars (first branches))
        arity0 (rule-vars-arity vars0)]
    (doseq [b    (next branches)
            :let [vars (:vars b)]]
      (when (not= arity0 (rule-vars-arity vars))
        (raise "Arity mismatch for rule '" (:symbol name) "': "
               (flatten-rule-vars vars0) " vs. " (flatten-rule-vars vars)
               {:error :parser/rule, :rule name})))))

(defn parse-rules [form]
  ;; group rule branches by name
  (let [name->branch (group-by :name (parse-seq parse-rule form))]
    (forv [[name branches] name->branch
           :let [branches (forv [b branches]
                            (datalog.parser.type.RuleBranch.
                             (:vars b) (:clauses b)))]]
      (do
        (validate-arity name branches)
        (datalog.parser.type.Rule. name branches)))))

(defn query->map [query]
  (loop [parsed {}
         key    nil
         qs     query]
    (if-let [[q & qs] (not-empty qs)]
      (if (keyword? q)
        (recur parsed q qs)
        (recur (update parsed key (fnil conj []) q) key qs))
      parsed)))

(defn assert-valid [q form]
  (let [find-vars    (t/collect-vars #{} (:qfind  q))
        with-vars    (set                (:qwith  q))
        in-vars      (t/collect-vars #{} (:qin    q))
        where-vars   (t/collect-vars #{} (:qwhere q))
        mapping-keys (t/collect-vars #{} (:qreturnmaps q))
        unknown      (set/difference (set/union find-vars with-vars)
                                     (set/union where-vars in-vars))
        shared       (set/intersection find-vars with-vars)
        mapped?      (or
                      (empty? mapping-keys)
                      (= (count mapping-keys) (count find-vars)))]
    (when-not (empty? unknown)
      (raise "Query for unknown vars: " (mapv :symbol unknown)
             {:error :parser/query, :vars unknown, :form form}))
    (when-not (empty? shared)
      (raise ":find and :with should not use same variables: " (mapv :symbol shared)
             {:error :parser/query, :vars shared, :form form}))
    (when-not mapped?
      (raise "Count of :keys/:strs/:syms must match count of :find"
             {:error :parser/query, :keys mapping-keys, :values in-vars, :form form})))

  (let [in-vars    (t/collect-vars        (:qin q))
        in-sources (collect-type SrcVar   (:qin q))
        in-rules   (collect-type RulesVar (:qin q))]
    (when-not (and (distinct? in-vars)
                   (distinct? in-sources)
                   (distinct? in-rules))
      (raise "Vars used in :in should be distinct"
             {:error :parser/query, :form form})))

  (let [with-vars (t/collect-vars (:qwith q))]
    (when-not (distinct? with-vars)
      (raise "Vars used in :with should be distinct"
             {:error :parser/query, :form form})))

  (let [in-sources    (collect-type SrcVar (:qin    q) #{})
        where-sources (collect-type SrcVar (:qwhere q) #{})
        unknown       (set/difference where-sources in-sources)]
    (when-not (empty? unknown)
      (raise "Where uses unknown source vars: " (mapv :symbol unknown)
             {:error :parser/query, :vars unknown, :form form})))

  (let [rule-exprs (collect-type RuleExpr (:qwhere q))
        rules-vars (collect-type RulesVar (:qin    q))]
    (when (and (not (empty? rule-exprs))
               (empty? rules-vars))
      (raise "Missing rules var '%' in :in"
             {:error :parser/query, :form form}))))
