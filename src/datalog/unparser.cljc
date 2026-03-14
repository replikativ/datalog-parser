(ns datalog.unparser
  (:require [datalog.parser.type :as t]
            #?(:cljs
               [datalog.parser.type :refer
                [Aggregate And BindColl BindIgnore BindScalar BindTuple Constant
                 DefaultSrc FindColl FindRel FindScalar FindTuple Function
                 MappingKey Not Or
                 Pattern Placeholder PlainSymbol Predicate Pull Query
                 ReturnMaps Rule RuleBranch RuleExpr RulesVar RuleVars
                 SrcVar Variable]]))
  #?(:clj
     (:import [datalog.parser.type
               Aggregate And BindColl BindIgnore BindScalar BindTuple Constant
               DefaultSrc FindColl FindRel FindScalar FindTuple Function
               MappingKey Not Or
               Pattern Placeholder PlainSymbol Predicate Pull Query
               ReturnMaps Rule RuleBranch RuleExpr RulesVar RuleVars
               SrcVar Variable])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol PUnparse
  (-unparse [this]))

(defn unparse
  "Converts a parsed query record back into the query vector DSL.
  Inverse of datalog.parser/parse."
  [v]
  (-unparse v))

(defn unparse-rules
  "Converts a collection of parsed Rule records back into the rules vector DSL.
  Inverse of datalog.parser/parse-rules."
  [rules]
  (vec (mapcat -unparse rules)))

(extend-protocol PUnparse
  Aggregate
  (-unparse [{:keys [fn args]}]
    (if (instance? Variable fn)
      (apply list 'aggregate (-unparse fn) (map -unparse args))
      (apply list (-unparse fn) (map -unparse args))))

  And
  (-unparse [{:keys [clauses]}]
    (apply list 'and (map -unparse clauses)))

  BindColl
  (-unparse [bc]
    [(-unparse (:binding bc)) '...])

  BindIgnore
  (-unparse [_] '_)

  BindScalar
  (-unparse [v]
    (-unparse (:variable v)))

  BindTuple
  (-unparse [bt]
    (mapv -unparse (:bindings bt)))

  Constant
  (-unparse [c] (:value c))

  DefaultSrc
  (-unparse [_] nil)

  FindColl
  (-unparse [{:keys [element]}]
    [[(-unparse element) '...]])

  FindRel
  (-unparse [fr]
    (mapv -unparse (:elements fr)))

  FindScalar
  (-unparse [s]
    [(-unparse (:element s)) '.])

  FindTuple
  (-unparse [{:keys [elements]}]
    [(mapv -unparse elements)])

  Function
  (-unparse [{:keys [fn args binding]}]
    [(apply list (-unparse fn) (map -unparse args))
     (-unparse binding)])

  MappingKey
  (-unparse [mk] (:mapping-key mk))

  Not
  (-unparse [{:keys [source vars clauses]}]
    (let [src (-unparse source)
          clause-vars (into #{} (distinct (t/collect-vars clauses)))
          not-join? (not= (set vars) clause-vars)]
      (apply list
             (concat
              (when src [src])
              (if not-join?
                ['not-join (mapv -unparse vars)]
                ['not])
              (map -unparse clauses)))))

  Or
  (-unparse [{:keys [source rule-vars clauses]}]
    (let [src (-unparse source)
          {:keys [required free]} rule-vars
          clause-vars (into #{} (distinct (t/collect-vars clauses)))
          or-join? (or required (not= (set free) clause-vars))]
      (apply list
             (concat
              (when src [src])
              (if or-join?
                ['or-join (vec (-unparse rule-vars))]
                ['or])
              (map -unparse clauses)))))

  Pattern
  (-unparse [{:keys [source pattern]}]
    (vec (concat (when-let [s (-unparse source)] [s])
                 (map -unparse pattern))))

  Placeholder
  (-unparse [_] '_)

  PlainSymbol
  (-unparse [s] (:symbol s))

  Predicate
  (-unparse [{:keys [fn args]}]
    [(apply list (-unparse fn) (map -unparse args))])

  Pull
  (-unparse [{:keys [source variable pattern]}]
    (let [src (-unparse source)]
      (if (= src '$)
        (list 'pull (-unparse variable) (-unparse pattern))
        (list 'pull src (-unparse variable) (-unparse pattern)))))

  Query
  (-unparse [{:keys [qfind qwith qin qwhere] :as q}]
    (let [qlimit (:qlimit q)
          qoffset (:qoffset q)
          qreturnmaps (:qreturnmaps q)]
      (vec
       (concat
        [:find] (-unparse qfind)
        (when (seq qwith)
          (into [:with] (map -unparse qwith)))
        (into [:in] (map -unparse qin))
        (when qreturnmaps
          (-unparse qreturnmaps))
        [:where]
        (map -unparse qwhere)
        (when qlimit [:limit qlimit])
        (when qoffset [:offset qoffset])))))

  ReturnMaps
  (-unparse [{:keys [mapping-type mapping-keys]}]
    (into [mapping-type] (map -unparse mapping-keys)))

  Rule
  (-unparse [{:keys [name branches]}]
    (let [n (-unparse name)]
      (mapv (fn [{:keys [vars clauses]}]
              (vec (cons (vec (cons n (-unparse vars)))
                         (map -unparse clauses))))
            branches)))

  RuleBranch
  (-unparse [{:keys [vars clauses]}]
    {:vars (-unparse vars) :clauses (mapv -unparse clauses)})

  RuleExpr
  (-unparse [{:keys [source name args]}]
    (let [src (-unparse source)]
      (apply list
             (concat (when src [src])
                     [(-unparse name)]
                     (map -unparse args)))))

  RulesVar
  (-unparse [_] '%)

  RuleVars
  (-unparse [{:keys [required free]}]
    (if required
      (concat [(mapv -unparse required)]
              (map -unparse free))
      (map -unparse free)))

  SrcVar
  (-unparse [s] (:symbol s))

  Variable
  (-unparse [v]
    (:symbol v)))
