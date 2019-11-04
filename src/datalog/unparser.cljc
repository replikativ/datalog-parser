(ns datalog.unparser
  (:require [datalog.parser :as parser]
            #?(:cljs
              [datalog.parser.type :refer
               [Aggregate And BindColl BindIgnore BindScalar BindTuple Constant
                DefaultSrc FindColl FindRel FindScalar FindTuple Function Not Or
                Pattern Placeholder PlainSymbol Predicate Pull Query Rule
                RuleBranch RuleExpr RulesVar RuleVars SrcVar Variable]]))
  #?(:clj
    (:import  [datalog.parser.type
               Aggregate And BindColl BindIgnore BindScalar BindTuple Constant
               DefaultSrc FindColl FindRel FindScalar FindTuple Function Not Or
               Pattern Placeholder PlainSymbol Predicate Pull Query Rule
               RuleBranch RuleExpr RulesVar RuleVars SrcVar Variable])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol PUnparse
  (-unparse [this]))

(defn unparse
  "Unparsing of datalog parser records back to the query DSL datastructure.
  Specifically it is the inverse to datalog.parser/parse-query."
  [v]
  (-unparse v))


;; ===== Missing Pull records

;; PullSpec
;; PullAttrName
;; PullReverseAttrName
;; PullLimitExpr
;; PullDefaultExpr
;; PullWildcard
;; PullRecursionLimit
;; PullMapSpecEntry
;; PullAttrWithOpts


(extend-protocol PUnparse
  Aggregate
  (-unparse [{:keys [fn args]}]
    (conj (map -unparse args)
          (-unparse fn)))

  ;; TODO test
  And
  (-unparse [{:keys [clauses]}]
    (concat (list 'and)
            (map -unparse clauses)))

  BindColl
  (-unparse [bc]
    [(-unparse (:binding bc)) '...])

  ;; TODO test
  BindIgnore
  (-unparse [v] '_)

  BindScalar
  (-unparse [v]
    (-unparse (:variable v)))

  BindTuple
  (-unparse [bt]
    (mapv -unparse (:bindings bt)))

  Constant
  (-unparse [c] (:value c))

  ;; TODO test
  DefaultSrc
  (-unparse [s])

  ;; TODO test
  FindColl
  (-unparse [{:keys [element]}]
    [[(-unparse element) '...]])

  FindRel
  (-unparse [fr]
    (map -unparse (:elements fr)))

  FindScalar
  (-unparse [s]
    [(-unparse (:element s)) '.])

  ;; TODO test
  FindTuple
  (-unparse [{:keys [elements]}]
    [(-unparse elements)])

  Function
  (-unparse [f]
    [(concat [(-unparse (:fn f))]
             (map -unparse (:args f)))
     (-unparse (:binding f))])

  ;; TODO test, check not-join
  Not
  (-unparse [{:keys [source vars clauses]}]
    (concat (when-let [src (-unparse source)]
              (list src 'not)
              (list 'not))
            (map -unparse clauses)))

  ;; TODO test, check or-join
  Or
  (-unparse [{:keys [source rule-vars clauses]}]
    (concat (list 'or)
            (map -unparse clauses)))

  Pattern
  (-unparse [{:keys [source pattern] :as p}]
    (concat [(-unparse source)] (map -unparse pattern)))

  ;; TODO test
  Placeholder
  (-unparse [_] '_)

  PlainSymbol
  (-unparse [s] (:symbol s))

  Predicate
  (-unparse [{:keys [fn args] :as p}]
    [(conj (map -unparse args)
           (-unparse fn))])

  ;; TODO test
  Pull
  (-unparse [{:keys [source variable pattern]}]
    (list 'pull
          (-unparse source) ;; TODO sugar remove $
          (-unparse variable)
          (-unparse pattern)))

  Query
  (-unparse [{:keys [qfind qwith qin qwhere]}]
    (vec
     (concat
      [:find] (-unparse qfind)
      (when-not (empty? qwith)
        (conj (map -unparse qwith) :with))
      (conj (map -unparse qin) :in)
      [:where]
      (mapv (comp vec -unparse) qwhere))))

  ;; TODO test
  Rule
  (-unparse [{:keys [name branches]}]
    (concat (list (-unparse name))
            (map -unparse branches)))

  ;; TODO test
  RuleBranch
  (-unparse [{:keys [vars clauses]}]
    (mapv -unparse clauses))

  ;; TODO test
  RuleExpr
  (-unparse [{:keys [source name args]}]
    [(-unparse source) (concat (list (-unparse name))
                               (map -unparse args))])

  ;; TODO test
  RulesVar
  (-unparse [s] '%)

  ;; TODO implement and test
  RuleVars
  (-unparse [{:keys [required free] :as v}]
    (throw (ex-info "Rule Vars not supported yet." {:v v})))

  SrcVar
  (-unparse [s] (:symbol s))

  Variable
  (-unparse [v]
    (:symbol v)))


