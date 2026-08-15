(ns datalog.parser
  (:require [datalog.parser.impl :as impl]
            [datalog.parser.type :as t]
            [datalog.parser.impl.util
             #?(:cljs :refer-macros :clj :refer) [raise]]))

#?(:clj (set! *warn-on-reflection* true))

(defn parse
  "Parses a query, given as a vector or a map, into a Query record.

  Options:

    :implicit-rules?  a rule expression is allowed without a '%' binding in
                      :in. Some engines pre-install rules, e.g. Datahike its
                      bitemporal ones, so a query calling them never binds '%'
                      itself. Either true, for any rule, or the collection of
                      rule names that are pre-installed."
  ([q] (parse q nil))
  ([q opts]
   (let [qm (cond
              (map? q) q
              (sequential? q) (impl/query->map q)
              :else (raise "Query should be a vector or a map"
                           {:error :parser/query, :form q}))
         res (t/map->Query {:qfind       (impl/parse-find (:find qm))
                            :qwith       (some-> qm :with impl/parse-with)
                            :qin         (impl/parse-in (:in qm ['$]))
                            :qwhere      (impl/parse-where (:where qm []))
                            :qhaving     (-> qm :having impl/parse-having)
                            :qlimit      (-> qm :limit impl/parse-limit)
                            :qoffset     (-> qm :offset impl/parse-offset)
                            :qorder      (-> qm :order-by impl/parse-order)
                            :qtimeout    (-> qm :timeout impl/parse-timeout)
                            :qreturnmaps (-> qm (select-keys [:keys :syms :strs])
                                             (impl/parse-return-maps))})]
     (impl/assert-valid res q qm opts)
     res)))

(defn parse-rules [rules]
  (impl/parse-rules rules))
