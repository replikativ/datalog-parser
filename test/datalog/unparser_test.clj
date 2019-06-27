(ns datalog.unparser-test
  (:require [datalog.unparser :refer [unparse]]
            [datalog.parser :refer [parse]]
            [clojure.test      :refer [deftest testing is] :as test])
  (:use [datalog.unparser]))

(let [q '[:find (sum ?balance-before)
          :in $before $after $txn $txs
          :where
          [(= ?balance-before 42)]
          [(d/q [:find (sum ?balance-before)
                 :in $before $after $txn $txs
                 :where
                 [(= ?balance-before 42)]]
                $before $after $txn $txs)]]]
  (deftest unparse-roundtrip-test
    (testing "Datahike query unparsing."
      (is (= q (unparse (parse q)))))))



