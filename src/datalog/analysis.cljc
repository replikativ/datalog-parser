(ns datalog.analysis
  "Static analysis of a parsed query, independent of any query engine.

  A clause such as `[(> ?a 18)]` cannot run before something binds `?a`, but
  the order in which the clauses are written does not matter, because an engine
  is free to reorder them. What does matter is whether any order can work at
  all, and that question is decidable from the parsed query alone."
  (:require [clojure.set :as set]
            [datalog.parser.impl :as impl]
            [datalog.parser.type :as t]
            #?(:cljs [datalog.parser.type :refer [And Function Not Or Pattern
                                                  Predicate RuleExpr]])
            [datalog.parser.impl.util
             #?(:cljs :refer-macros :clj :refer) [raise]])
  #?(:clj
     (:import [datalog.parser.type
               And Function Not Or Pattern Predicate RuleExpr])))

#?(:clj (set! *warn-on-reflection* true))

(defn- var-symbols [form]
  (into #{} (map :symbol) (t/collect-vars #{} form)))

(defn- clause-demands
  "What a clause needs bound before it can run, and what it can bind itself.

  Nested clauses are flattened into the same pool. That is the permissive
  reading: a binding made inside a branch is offered to the whole query, so an
  ordering that an engine could still satisfy is never reported as impossible.
  Only 'not' and 'or' get this treatment, they neither require nor guarantee
  bindings of their own."
  [clause]
  (cond
    (instance? Predicate clause) [{:clause   clause
                                   :requires (var-symbols (:args clause))
                                   :binds    #{}}]
    (instance? Function clause)  [{:clause   clause
                                   :requires (var-symbols (:args clause))
                                   :binds    (var-symbols (:binding clause))}]
    (instance? Pattern clause)   [{:clause   clause
                                   :requires #{}
                                   :binds    (var-symbols clause)}]
    (instance? RuleExpr clause)  [{:clause   clause
                                   :requires #{}
                                   :binds    (var-symbols (:args clause))}]
    (or (instance? Not clause)
        (instance? Or clause)
        (instance? And clause)) (into [] (mapcat clause-demands) (:clauses clause))
    :else                       []))

(defn- saturate
  "Binds what can be bound until nothing new is, then reports what is left.

  Binding is monotone, running a clause never takes a binding away, so the set
  of variables reachable by any ordering has a single fixpoint and reaching it
  needs no search over orderings."
  [{:keys [qin qwhere]}]
  (loop [bound   (var-symbols qin)
         pending (into [] (mapcat clause-demands) qwhere)]
    (let [{ready   true
           blocked false} (group-by #(set/subset? (:requires %) bound) pending)]
      (if (seq ready)
        (recur (into bound (mapcat :binds) ready) (vec blocked))
        [bound (vec blocked)]))))

(defn bindable-vars
  "The variables of a parsed query that some ordering of its clauses can bind,
  as a set of symbols. Everything the `:where` clauses use beyond these is
  unbindable, whatever the engine does."
  [parsed-query]
  (first (saturate parsed-query)))

(defn unsatisfiable-clauses
  "The clauses of a parsed query that no ordering can ever run, as a vector of
  `{:clause <parsed clause>, :form <as written>, :missing #{symbols}}`. Empty
  when the query is satisfiable."
  [parsed-query]
  (let [[bound blocked] (saturate parsed-query)]
    (mapv (fn [{:keys [clause requires]}]
            {:clause  clause
             :form    (impl/get-source clause)
             :missing (set/difference requires bound)})
          blocked)))

(defn assert-satisfiable
  "Returns the parsed query, or raises when a clause needs a variable that no
  ordering of the query can bind."
  [parsed-query]
  (when-some [blocked (not-empty (unsatisfiable-clauses parsed-query))]
    (let [missing (into (sorted-set) (mapcat :missing) blocked)]
      (raise "No clause of the query can bind: " (vec missing)
             {:error   :analysis/unsatisfiable
              :missing missing
              :clauses (mapv :form blocked)})))
  parsed-query)
