(ns datalog.parser
  (:require [datalog.parser.impl :as impl]
            [datalog.parser.type :as t]
            [datalog.parser.impl.util
             #?(:cljs :refer-macros :clj :refer) [raise]]))

#?(:clj (set! *warn-on-reflection* true))

(defn parse [q]
  (let [qm (cond
             (map? q) q
             (sequential? q) (impl/query->map q)
             :else (raise "Query should be a vector or a map"
                          {:error :parser/query, :form q}))
        res (t/map->Query {:qfind       (impl/parse-find (:find qm))
                           :qwith       (some-> qm :with impl/parse-with)
                           :qin         (impl/parse-in (:in qm ['$]))
                           :qwhere      (impl/parse-where (:where qm []))
                           :qlimit      (-> qm :limit impl/parse-limit)
                           :qoffset     (-> qm :offset impl/parse-offset)
                           :qreturnmaps (-> qm (select-keys [:keys :syms :strs])
                                               (impl/parse-return-maps))})]
    (impl/assert-valid res q)
    res))
