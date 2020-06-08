(ns datalog.parser.type
  (:require [datalog.parser.impl.proto :as p]
            [datalog.parser.util       :as util]
            [datalog.parser.impl.util
             :refer [collect]])
  #?(:cljs (:require-macros [datalog.parser.type :refer [deftrecord]])))

#?(:clj (set! *warn-on-reflection* true))

(declare collect-vars)

#?(:clj
   (defmacro deftrecord
     "Augment all parser records with default implementation of ITraversable"
     [tagname fields & rest]
     (let [[f pred acc]  (map gensym ["f" "pred" "acc"])
           walked-fields (map #(list `util/postwalk % f) fields)]
       `(defrecord ~tagname ~fields
          p/Traversable
          (~'-traversable? [_#] true)
          p/ITraversable
          (~'-postwalk [this# ~f]
           (-> (new ~tagname ~@walked-fields)
               (vary-meta merge (meta this#))))
          (~'-collect [_# ~pred ~acc]
           ~(reduce #(list `collect pred %2 %1) acc fields))
          (~'-collect-vars [_# ~acc]
           ~(reduce #(list `collect-vars %1 %2) acc fields))
          ~@rest))))

;; placeholder    = the symbol '_'
;; variable       = symbol starting with "?"
;; src-var        = symbol starting with "$"
;; rules-var      = the symbol "%"
;; constant       = any non-variable data literal
;; plain-symbol   = symbol that does not begin with "$" or "?"
;; mapping-key    = key for mapping the result

(deftrecord Placeholder [])
(deftrecord Variable    [symbol])
(deftrecord SrcVar      [symbol])
(deftrecord DefaultSrc  [])
(deftrecord RulesVar    [])
(deftrecord Constant    [value])
(deftrecord PlainSymbol [symbol])

(deftrecord RuleVars [required free])

(def variable? (partial instance? Variable))

;; binding        = (bind-scalar | bind-tuple | bind-coll | bind-rel)
;; bind-scalar    = variable
;; bind-tuple     = [ (binding | '_')+ ]
;; bind-coll      = [ binding '...' ]
;; bind-rel       = [ [ (binding | '_')+ ] ]

(deftrecord BindIgnore [])
(deftrecord BindScalar [variable])
(deftrecord BindTuple  [bindings])
(deftrecord BindColl   [binding])

;; find-spec        = ':find' (find-rel | find-coll | find-tuple | find-scalar)
;; find-rel         = find-elem+
;; find-coll        = [ find-elem '...' ]
;; find-scalar      = find-elem '.'
;; find-tuple       = [ find-elem+ ]
;; find-elem        = (variable | pull-expr | aggregate | custom-aggregate)
;; pull-expr        = [ 'pull' src-var? variable pull-pattern ]
;; pull-pattern     = (constant | variable | plain-symbol)
;; aggregate        = [ aggregate-fn fn-arg+ ]
;; aggregate-fn     = plain-symbol
;; custom-aggregate = [ 'aggregate' variable fn-arg+ ]

(deftrecord Aggregate [fn args])
(deftrecord Pull      [source variable pattern])

(deftrecord FindRel    [elements])
(deftrecord FindColl   [element])
(deftrecord FindScalar [element])
(deftrecord FindTuple  [elements])

(extend-protocol p/IFindVars
  Variable
  (p/-find-vars [this] [(.-symbol this)])
  Aggregate
  (p/-find-vars [this] (p/-find-vars (last (.-args this))))
  Pull
  (p/-find-vars [this] (p/-find-vars (.-variable this))))

(extend-protocol p/IFindElements
  FindRel
  (p/find-elements [this] (.-elements this))
  FindColl
  (p/find-elements [this] [(.-element this)])
  FindScalar
  (p/find-elements [this] [(.-element this)])
  FindTuple
  (p/find-elements [this] (.-elements this)))

;; clause          = (data-pattern | pred-expr | fn-expr | rule-expr | not-clause | not-join-clause | or-clause | or-join-clause)
;; data-pattern    = [ src-var? (variable | constant | '_')+ ]
;; pred-expr       = [ [ pred fn-arg+ ] ]
;; pred            = (plain-symbol | variable)
;; fn-expr         = [ [ fn fn-arg+ ] binding ]
;; fn              = (plain-symbol | variable)
;; rule-expr       = [ src-var? rule-name (variable | constant | '_')+ ]
;; not-clause      = [ src-var? 'not' clause+ ]
;; not-join-clause = [ src-var? 'not-join' [ variable+ ] clause+ ]
;; or-clause       = [ src-var? 'or' (clause | and-clause)+ ]
;; or-join-clause  = [ src-var? 'or-join' rule-vars (clause | and-clause)+ ]
;; and-clause      = [ 'and' clause+ ]

(deftrecord Pattern   [source pattern])
(deftrecord Predicate [fn args])
(deftrecord Function  [fn args binding])
(deftrecord RuleExpr  [source name args]) ;; TODO rule with constant or '_' as argument
(deftrecord Not       [source vars clauses])
(deftrecord Or        [source rule-vars clauses])
(deftrecord And       [clauses])

(def not? (partial instance? Not))
(def or?  (partial instance? Or))

;; rule-branch = [rule-head clause+]
;; rule-head   = [rule-name rule-vars]
;; rule-name   = plain-symbol

(deftrecord RuleBranch [vars clauses])
(deftrecord Rule       [name branches])

;; return maps need one or many keys to map to the result
;; mapping-key    = plain-symbol
;; mapping-type   = keyword
;; return-mapping = {mapping-type [mapping-keys]}

(deftrecord MappingKey [mapping-key])
(deftrecord ReturnMaps [mapping-type mapping-keys])

(def return-maps? (partial instance? ReturnMaps))

;; q* prefix because of https"//dev.clojure.org/jira/browse/CLJS-2237"
(deftrecord Query [qfind qwith qin qwhere])

(defn collect-vars
  ([form] (collect-vars [] form))
  ([acc form]
   (cond
     (variable?       form)  (conj acc form)
     (not?            form)  (into acc (:vars form))
     (or?             form)  (collect-vars acc (:rule-vars form))
     (return-maps?    form)  (into acc (:mapping-keys form))
     (p/-traversable? form)  (p/-collect-vars form acc)
     (sequential?     form)  (reduce collect-vars acc form)
     :else                  acc)))
