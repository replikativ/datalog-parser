(ns datalog.analysis-test
  (:require #?(:cljs [cljs.test :refer-macros [are deftest is testing]]
               :clj  [clojure.test :refer [are deftest is testing]])
            [datalog.analysis :as analysis]
            [datalog.parser :as parser])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(defn- unsatisfiable [q]
  (analysis/unsatisfiable-clauses (parser/parse q)))

(defn- bindable [q]
  (analysis/bindable-vars (parser/parse q)))

(deftest satisfiable-queries
  (testing "nothing is reported for a query some ordering can run"
    (are [q] (empty? (unsatisfiable q))
      '[:find ?e :in $ :where [?e :age ?a] [(> ?a 18)]]

      ;; the written order does not matter, an engine may reorder
      '[:find ?e :in $ :where [(> ?a 18)] [?e :age ?a]]

      ;; a chain of functions, written backwards
      '[:find ?c :in $ ?a :where [(inc ?b) ?c] [(inc ?a) ?b]]

      ;; the input provides the binding
      '[:find ?e :in $ ?min :where [?e :age ?a] [(> ?a ?min)]]

      ;; a function that binds without needing anything
      '[:find ?x :in $ :where [(range 1 10) [?x ...]]]

      ;; branches of an 'or' bind for the clauses that follow
      '[:find ?e :in $ :where (or [?e :a 1] [?e :b 2]) [(> ?e 1)]]

      ;; a rule expression binds its arguments
      '[:find ?e :in $ % :where (friends ?e ?f) [(> ?f 1)]]

      ;; bindings destructured out of an input
      '[:find ?e :in $ [[?a ?b]] :where [?e ?a ?b] [(> ?b 1)]]

      ;; a 'not' neither requires nor withholds bindings
      '[:find ?e :in $ :where [?e :age ?a] (not [?e :blocked true]) [(> ?a 1)]])))

(deftest unsatisfiable-queries
  (testing "a predicate over a variable that nothing binds"
    (let [[{:keys [form missing]} :as blocked]
          (unsatisfiable '[:find ?e :in $ :where [?e :age ?a] [(> ?b 18)]])]
      (is (= 1 (count blocked)))
      (is (= '[(> ?b 18)] form))
      (is (= '#{?b} missing))))

  (testing "functions that need each other are blocked in either order"
    (let [blocked (unsatisfiable '[:find ?x :in $ :where [(inc ?y) ?x] [(inc ?x) ?y]])]
      (is (= '[[(inc ?y) ?x] [(inc ?x) ?y]] (mapv :form blocked)))
      (is (= '[#{?y} #{?x}] (mapv :missing blocked)))))

  (testing "a clause nested in a branch is reported too"
    (is (= '[#{?c}]
           (mapv :missing
                 (unsatisfiable
                  '[:find ?e :in $
                    :where (or-join [?e] (and [?e :a 1] [(> ?c 1)]) [?e :b 2])]))))))

(deftest bindable-variables
  (are [q result] (= result (bindable q))
    '[:find ?e :in $ :where [?e :age ?a]]
    '#{?e ?a}

    '[:find ?e :in $ ?min :where [?e :age ?a] [(> ?a ?min)]]
    '#{?e ?a ?min}

    ;; ?y is unreachable, so ?x, which needs it, is not bindable either
    '[:find ?x :in $ :where [(inc ?y) ?x]]
    '#{}))

(deftest assert-satisfiable-passes-the-query-through
  (let [q (parser/parse '[:find ?e :in $ :where [?e :age ?a] [(> ?a 18)]])]
    (is (= q (analysis/assert-satisfiable q))))

  (is (thrown-with-msg?
       ExceptionInfo #"No clause of the query can bind: \[\?b\]"
       (analysis/assert-satisfiable
        (parser/parse '[:find ?e :in $ :where [?e :age ?a] [(> ?b 18)]])))))
