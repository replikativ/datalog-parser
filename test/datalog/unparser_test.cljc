(ns datalog.unparser-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest testing is]]
               :clj  [clojure.test :refer     [deftest testing is]])
            [datalog.parser :as parser]
            [datalog.unparser :as unparser]))

(defn roundtrip [q]
  (unparser/unparse (parser/parse q)))

(defn roundtrip-rules [rules]
  (unparser/unparse-rules (parser/parse-rules rules)))

;; === Query roundtrips ===

(deftest simple-query-roundtrip
  (let [q '[:find ?e
            :in $
            :where [?e :name "Ivan"]]]
    (is (= q (roundtrip q)))))

(deftest multi-variable-find
  (let [q '[:find ?e ?name
            :in $
            :where [?e :name ?name]]]
    (is (= q (roundtrip q)))))

(deftest multiple-sources
  (let [q '[:find (sum ?balance-before) ?balance-before
            :in $before $after $txn $txs
            :where
            [(= ?balance-before 42)]]]
    (is (= q (roundtrip q)))))

(deftest with-clause
  (let [q '[:find ?e
            :with ?x
            :in $
            :where [?e :name ?x]]]
    (is (= q (roundtrip q)))))

(deftest find-scalar
  (let [q '[:find ?e .
            :in $
            :where [?e :name "Ivan"]]]
    (is (= q (roundtrip q)))))

(deftest find-coll
  (let [q '[:find [?e ...]
            :in $
            :where [?e :name "Ivan"]]]
    (is (= q (roundtrip q)))))

(deftest find-tuple
  (let [q '[:find [?e ?name]
            :in $
            :where [?e :name ?name]]]
    (is (= q (roundtrip q)))))

(deftest predicate-clause
  (let [q '[:find ?e
            :in $
            :where
            [?e :age ?a]
            [(> ?a 18)]]]
    (is (= q (roundtrip q)))))

(deftest function-clause
  (let [q '[:find ?e ?str
            :in $
            :where
            [?e :name ?name]
            [(str ?name "-suffix") ?str]]]
    (is (= q (roundtrip q)))))

(deftest aggregate-in-find
  (let [q '[:find (count ?e)
            :in $
            :where [?e :name _]]]
    (is (= q (roundtrip q)))))

(deftest aggregate-multi-arg
  (let [q '[:find (sum ?balance) ?currency
            :in $
            :where
            [?e :balance ?balance]
            [?e :currency ?currency]]]
    (is (= q (roundtrip q)))))

(deftest custom-aggregate
  (let [q '[:find (aggregate ?f ?a)
            :in $ ?f
            :where [?e :val ?a]]]
    (is (= q (roundtrip q)))))

(deftest not-clause
  (let [q '[:find ?e
            :in $
            :where
            [?e :name ?name]
            (not [?e :blocked true])]]
    (is (= q (roundtrip q)))))

(deftest not-join-clause
  (let [q '[:find ?e
            :in $
            :where
            [?e :name ?name]
            (not-join [?e]
                      [?e :follows ?x]
                      [?x :blocked true])]]
    (is (= q (roundtrip q)))))

(deftest not-with-source
  (let [q '[:find ?e
            :in $1
            :where
            [$1 ?e :name ?name]
            ($1 not [?e :blocked true])]]
    (is (= q (roundtrip q)))))

(deftest or-clause
  (let [q '[:find ?e
            :in $
            :where
            (or [?e :name "Ivan"]
                [?e :name "Peter"])]]
    (is (= q (roundtrip q)))))

(deftest or-join-clause
  (let [q '[:find ?e
            :in $
            :where
            (or-join [?e]
                     [?e :name "Ivan"]
                     [?e :age ?a])]]
    (is (= q (roundtrip q)))))

(deftest or-join-with-required
  (let [q '[:find ?e
            :in $
            :where
            (or-join [[?e]]
                     (and [?e :name "Ivan"]
                          [?e :age 30]))]]
    (is (= q (roundtrip q)))))

(deftest or-with-and
  (let [q '[:find ?e
            :in $
            :where
            (or [?e :name "Ivan"]
                (and [?e :name "Peter"]
                     [?e :age 42]))]]
    (is (= q (roundtrip q)))))

(deftest or-with-source
  (let [q '[:find ?e
            :in $1
            :where
            ($1 or [?e :name "Ivan"]
                   [?e :name "Peter"])]]
    (is (= q (roundtrip q)))))

(deftest rule-expr-clause
  (let [q '[:find ?e
            :in $ %
            :where
            [?e :name ?name]
            (friends ?e ?other)]]
    (is (= q (roundtrip q)))))

(deftest rule-expr-with-source
  (let [q '[:find ?e
            :in $1 %
            :where
            ($1 friends ?e ?other)]]
    (is (= q (roundtrip q)))))

(deftest pull-expression
  (let [q '[:find (pull ?e [:name :age])
            :in $
            :where [?e :name _]]]
    (is (= q (roundtrip q)))))

(deftest pull-with-source
  (let [q '[:find (pull $1 ?e [:name :age])
            :in $1
            :where [$1 ?e :name _]]]
    (is (= q (roundtrip q)))))

(deftest return-maps-keys
  (let [q '[:find ?e
            :in $ ?fname ?lname
            :keys foo
            :where
            [?e :user/firstName ?fname]
            [?e :user/lastName ?lname]]]
    (is (= q (roundtrip q)))))

(deftest return-maps-strs
  (let [q '[:find ?e
            :in $ ?fname
            :strs bar
            :where [?e :name ?fname]]]
    (is (= q (roundtrip q)))))

(deftest return-maps-syms
  (let [q '[:find ?e
            :in $ ?fname
            :syms baz
            :where [?e :name ?fname]]]
    (is (= q (roundtrip q)))))

(deftest bind-coll-in-clause
  (let [q '[:find ?e
            :in $ [?name ...]
            :where [?e :name ?name]]]
    (is (= q (roundtrip q)))))

(deftest bind-tuple-in-clause
  (let [q '[:find ?e
            :in $ [?fname ?lname]
            :where
            [?e :firstName ?fname]
            [?e :lastName ?lname]]]
    (is (= q (roundtrip q)))))

(deftest bind-rel-in-clause
  (testing "bind-rel [[x y]] normalizes to bind-coll [[x y] ...] after roundtrip"
    (let [q-in  '[:find ?e
                  :in $ [[?fname ?lname]]
                  :where
                  [?e :firstName ?fname]
                  [?e :lastName ?lname]]
          q-out '[:find ?e
                  :in $ [[?fname ?lname] ...]
                  :where
                  [?e :firstName ?fname]
                  [?e :lastName ?lname]]]
      (is (= q-out (roundtrip q-in))))))

(deftest pattern-with-source
  (let [q '[:find ?e
            :in $1
            :where [$1 ?e :name "Ivan"]]]
    (is (= q (roundtrip q)))))

(deftest pattern-with-placeholder
  (let [q '[:find ?e
            :in $
            :where [?e _ ?v]]]
    (is (= q (roundtrip q)))))

(deftest pattern-with-constant-values
  (let [q '[:find ?e
            :in $
            :where [?e :age 30]]]
    (is (= q (roundtrip q)))))

;; === Rules roundtrips ===

(deftest simple-rules-roundtrip
  (let [rules '[[(ancestor ?e1 ?e2)
                 [?e1 :ancestor ?e2]]
                [(ancestor ?e1 ?e2)
                 [?e1 :ancestor ?t]
                 (ancestor ?t ?e2)]]]
    (is (= rules (roundtrip-rules rules)))))

(deftest rules-with-required-vars
  (let [rules '[[(rule [?x] ?y)
                 [?x :name ?y]]]]
    (is (= rules (roundtrip-rules rules)))))

(deftest rules-with-only-required-vars
  (let [rules '[[(rule [?x])
                 [?x :name "Peter"]]]]
    (is (= rules (roundtrip-rules rules)))))

(deftest rules-with-multiple-required-and-free
  (let [rules '[[(rule [?x ?y] ?a ?b)
                 [?x :name ?y]
                 [?a :name ?b]]]]
    (is (= rules (roundtrip-rules rules)))))

(deftest rules-multiple-names
  (let [rules '[[(friends ?x ?y)
                 [?x :friend ?y]]
                [(enemies ?x ?y)
                 [?x :enemy ?y]]]]
    (is (= (set (roundtrip-rules rules))
           (set rules)))))

(deftest rules-multiple-branches
  (let [rules '[[(path ?x ?y)
                 [?x :edge ?y]]
                [(path ?x ?y)
                 [?x :edge ?z]
                 (path ?z ?y)]]]
    (is (= rules (roundtrip-rules rules)))))
