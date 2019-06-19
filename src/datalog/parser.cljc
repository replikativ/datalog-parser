(ns datalog.parser
  (:require [datalog.parser.impl :as impl]
            [datalog.parser.type :as t]
            [datalog.parser.impl.util
             #?(:cljs :refer-macros :clj :refer) [raise]]))

(defn parse [q]
  (let [qm  (cond
              (map?        q) q
              (sequential? q) (impl/query->map q)
              :else           (raise "Query should be a vector or a map"
                                     {:error :parser/query, :form q}))
        res (t/map->Query {:qfind  (impl/parse-find (:find qm))
                           :qwith  (some-> qm :with impl/parse-with)
                           :qin    (impl/parse-in    (:in    qm ['$]))
                           :qwhere (impl/parse-where (:where qm []))})]
    (impl/assert-valid res q)
    res))
