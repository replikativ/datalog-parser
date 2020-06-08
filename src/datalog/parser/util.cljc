(ns datalog.parser.util
  (:require [datalog.parser.impl.proto :as proto]))

#?(:clj (set! *warn-on-reflection* true))

(defn postwalk [form f]
  ;; additional handling for maps and records that keeps structure type
  (cond
    (proto/-traversable? form) (f (proto/-postwalk form f))
    (map? form)         (f (reduce-kv
                            (fn map-walker [form k v]
                              (assoc form k (postwalk v f)))
                            form form))
    ;; rest comes from clojure.core
    (seq? form)         (f (map #(postwalk % f) form))
    (coll? form)        (f (into (empty form) (map #(postwalk % f) form)))
    :else               (f form)))
