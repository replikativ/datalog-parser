(ns datalog.analysis.proto
  (:require [datalog.parser.type :as t]))

(defprotocol IInitialModing
  (-initial-modes [clause])
  (-required [clause])
  (-binds [clause]))

(extend-protocol IInitialModing
  t/Pattern
  (-initial-modes [clause] (let [vars (mapv :symbol (t/collect-vars [] clause))]
                             {:form clause
                              :vars vars
                              :modes #{#{}}}))
  (-required [_clause] #{})
  (-binds [clause] (set (map :symbol (t/collect-vars [] clause))))
  t/Function
  (-initial-modes [clause] (let [vars (mapv :symbol (:vars clause))
                                 bindings (mapv :symbol (:binding clause))
                                 all-vars (concat vars bindings)]
                             {:form clause
                              :vars all-vars
                              :modes #{(set (range (count vars)))}}))
  (-required [clause] (set (map :symbol (:vars clause))))
  (-binds [clause] (set (map :symbol (:binding clause))))
  t/Predicate
  (-initial-modes [clause] (let [vars (mapv :symbol (:vars clause))]
                             {:form clause
                              :vars vars
                              :modes #{(set (range (count vars)))}}))
  (-required [clause] (set (map :symbol (:vars clause))))
  (-binds [_clause] #{}))