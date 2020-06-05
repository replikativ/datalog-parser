(ns datalog.parser.impl.proto)

#?(:clj (set! *warn-on-reflection* true))

(defprotocol ITraversable
  (-collect      [_ pred acc])
  (-collect-vars [_ acc])
  (-postwalk     [_ f]))

(defprotocol Traversable
  (-traversable? [_]))

(defprotocol IFindVars
  (-find-vars [this]))

(defprotocol IFindElements
  (find-elements [this]))

(extend-type #?(:clj Object :cljs object)
  Traversable
  (-traversable? [_] false))

(extend-type nil
  Traversable
  (-traversable? [_] false))
