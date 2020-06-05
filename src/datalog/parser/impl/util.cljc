(ns ^:no-doc datalog.parser.impl.util
  (:require [datalog.parser.impl.proto :as proto]
            [datalog.parser.util       :as util]
            [clojure.string            :as str])
  (:refer-clojure :exclude [seqable?]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defmacro raise [& fragments]
     (let [msgs (for [m (butlast fragments)]
                  (cond->> m (not (string? m)) (list 'pr-str)))]
       `(throw (ex-info (str ~@msgs) ~(last fragments))))))

#?(:clj
   (defmacro forv [& for-args]
     `(into [] (for ~@for-args))))

(defn- decompose-ref [v]
  (cond
    (keyword? v) [(namespace v) (name v)]
    (string?  v) (recur (keyword v))
    :else        (raise "Bad attribute type: " v ", expected keyword or string"
                        {:error     :transact/syntax
                         :attribute v})))

(defn- reverse-attr? [v]
  (str/starts-with? v "_"))

(defn #?@(:clj  [^Boolean reverse-ref?]
          :cljs [^boolean reverse-ref?]) [attr]
  (-> attr decompose-ref second reverse-attr?))

(defn- invert-name [s]
  (if (reverse-attr? s)
    (subs s  1)
    (str "_" s)))

(defn reverse-ref [v]
  (-> (decompose-ref v)
      (update 1 invert-name)
      (cond->> (keyword? v) (apply keyword))))

(defn prefixed-symbol? [sym prefix]
  (and (symbol? sym) (= (first (name sym)) prefix)))

(defn- #?@(:clj  [^Boolean seqable?]
           :cljs [^boolean seqable?])
  [x]
  (and (not (string? x))
       #?(:cljs (or (cljs.core/seqable? x)
                    (array? x))
          :clj  (clojure.core/seqable? x)
          ;; was
          #_(or (seq? x)
                    (instance? clojure.lang.Seqable x)
                    (nil? x)
                    (instance? Iterable x)
                    (-> x .getClass .isArray)
                    (instance? java.util.Map x)))))



(defn collect
  ([pred form] (collect pred form []))
  ([pred form acc]
   (cond
     (pred form)         (conj acc form)
     (proto/-traversable? form) (proto/-collect form pred acc)
     (seqable? form)     (reduce
                          (fn collector [acc form]
                            (collect pred form acc))
                          acc form)
     :else               acc)))
