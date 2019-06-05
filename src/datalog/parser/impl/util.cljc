(ns ^:no-doc datalog.parser.impl.util
  (:require [clojure.string :as str]))

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
  (and (symbol? sym) (= (first (name sym) prefix))))
