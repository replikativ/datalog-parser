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

(extend-type nil
  Traversable
  (-traversable? [_] false))

#?(:clj (extend-protocol Traversable
          Object (-traversable? [_] false))

   :cljs (extend-protocol Traversable
           boolean (-traversable? [_] false)
           number  (-traversable? [_] false)
           object  (-traversable? [_] false)
           string  (-traversable? [_] false)))
