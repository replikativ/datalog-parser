(ns datalog.parser-test
  (:require #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
               :clj  [clojure.test :as t :refer        [is are deftest testing]])
            [datalog.parser :as dp])
  (:import [clojure.lang ExceptionInfo]))

;; utils
#?(:clj
(defmethod t/assert-expr 'thrown-msg? [msg form]
  (let [[_ match & body] form]
    `(try ~@body
          (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch Throwable e#
            (let [m# (.getMessage e#)]
              (if (= ~match m#)
                (t/do-report {:type :pass, :message ~msg, :expected '~form, :actual e#})
                (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual e#})))
            e#)))))

(deftest bindings
  (are [form res] (= (dp/parse-binding form) res)
    '?x
    (dp/->BindScalar (dp/->Variable '?x))

    '_
    (dp/->BindIgnore)

    '[?x ...]
    (dp/->BindColl (dp/->BindScalar (dp/->Variable '?x)))

    '[?x]
    (dp/->BindTuple [(dp/->BindScalar (dp/->Variable '?x))])

    '[?x ?y]
    (dp/->BindTuple [(dp/->BindScalar (dp/->Variable '?x)) (dp/->BindScalar (dp/->Variable '?y))])

    '[_ ?y]
    (dp/->BindTuple [(dp/->BindIgnore) (dp/->BindScalar (dp/->Variable '?y))])

    '[[_ [?x ...]] ...]
    (dp/->BindColl
      (dp/->BindTuple [(dp/->BindIgnore)
                      (dp/->BindColl
                        (dp/->BindScalar (dp/->Variable '?x)))]))

    '[[?a ?b ?c]]
    (dp/->BindColl
      (dp/->BindTuple [(dp/->BindScalar (dp/->Variable '?a))
                      (dp/->BindScalar (dp/->Variable '?b))
                      (dp/->BindScalar (dp/->Variable '?c))])))

    (is (thrown-with-msg? ExceptionInfo #"Cannot parse binding"
          (dp/parse-binding :key))))

(deftest in
  (are [form res] (= (dp/parse-in form) res)
    '[?x]
    [(dp/->BindScalar (dp/->Variable '?x))]

    '[$ $1 % _ ?x]
    [(dp/->BindScalar (dp/->SrcVar '$))
     (dp/->BindScalar (dp/->SrcVar '$1))
     (dp/->BindScalar (dp/->RulesVar))
     (dp/->BindIgnore)
     (dp/->BindScalar (dp/->Variable '?x))]

    '[$ [[_ [?x ...]] ...]]
    [(dp/->BindScalar (dp/->SrcVar '$))
     (dp/->BindColl
       (dp/->BindTuple [(dp/->BindIgnore)
                       (dp/->BindColl
                         (dp/->BindScalar (dp/->Variable '?x)))]))])

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse binding"
        (dp/parse-in ['?x :key]))))

(deftest with
  (is (= (dp/parse-with '[?x ?y])
         [(dp/->Variable '?x) (dp/->Variable '?y)]))

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse :with clause"
        (dp/parse-with '[?x _]))))

(deftest test-parse-find
  (is (= (dp/parse-find '[?a ?b])
         (dp/->FindRel [(dp/->Variable '?a) (dp/->Variable '?b)])))
  (is (= (dp/parse-find '[[?a ...]])
         (dp/->FindColl (dp/->Variable '?a))))
  (is (= (dp/parse-find '[?a .])
         (dp/->FindScalar (dp/->Variable '?a))))
  (is (= (dp/parse-find '[[?a ?b]])
         (dp/->FindTuple [(dp/->Variable '?a) (dp/->Variable '?b)]))))

(deftest test-parse-aggregate
  (is (= (dp/parse-find '[?a (count ?b)])
         (dp/->FindRel [(dp/->Variable '?a) (dp/->Aggregate (dp/->PlainSymbol 'count) [(dp/->Variable '?b)])])))
  (is (= (dp/parse-find '[[(count ?a) ...]])
         (dp/->FindColl (dp/->Aggregate (dp/->PlainSymbol 'count) [(dp/->Variable '?a)]))))
  (is (= (dp/parse-find '[(count ?a) .])
         (dp/->FindScalar (dp/->Aggregate (dp/->PlainSymbol 'count) [(dp/->Variable '?a)]))))
  (is (= (dp/parse-find '[[(count ?a) ?b]])
         (dp/->FindTuple [(dp/->Aggregate (dp/->PlainSymbol 'count) [(dp/->Variable '?a)]) (dp/->Variable '?b)]))))

(deftest test-parse-custom-aggregates
  (is (= (dp/parse-find '[(aggregate ?f ?a)])
         (dp/->FindRel [(dp/->Aggregate (dp/->Variable '?f) [(dp/->Variable '?a)])])))
  (is (= (dp/parse-find '[?a (aggregate ?f ?b)])
         (dp/->FindRel [(dp/->Variable '?a) (dp/->Aggregate (dp/->Variable '?f) [(dp/->Variable '?b)])])))
  (is (= (dp/parse-find '[[(aggregate ?f ?a) ...]])
         (dp/->FindColl (dp/->Aggregate (dp/->Variable '?f) [(dp/->Variable '?a)]))))
  (is (= (dp/parse-find '[(aggregate ?f ?a) .])
         (dp/->FindScalar (dp/->Aggregate (dp/->Variable '?f) [(dp/->Variable '?a)]))))
  (is (= (dp/parse-find '[[(aggregate ?f ?a) ?b]])
         (dp/->FindTuple [(dp/->Aggregate (dp/->Variable '?f) [(dp/->Variable '?a)]) (dp/->Variable '?b)]))))

(deftest test-parse-find-elements
  (is (= (dp/parse-find '[(count ?b 1 $x) .])
         (dp/->FindScalar (dp/->Aggregate (dp/->PlainSymbol 'count)
                                          [(dp/->Variable '?b)
                                           (dp/->Constant 1)
                                           (dp/->SrcVar '$x)])))))

(deftest validation
  (are [q msg] (thrown-msg? msg (dp/parse-query q))
    '[:find ?e :where [?x]]
    "Query for unknown vars: [?e]"

    '[:find ?e :with ?f :where [?e]]
    "Query for unknown vars: [?f]"

    '[:find ?e ?x ?t :in ?x :where [?e]]
    "Query for unknown vars: [?t]"

    '[:find ?x ?e :with ?y ?e :where [?x ?e ?y]]
    ":find and :with should not use same variables: [?e]"

    '[:find ?e :in $ $ ?x :where [?e]]
    "Vars used in :in should be distinct"

    '[:find ?e :in ?x $ ?x :where [?e]]
    "Vars used in :in should be distinct"

    '[:find ?e :in $ % ?x % :where [?e]]
    "Vars used in :in should be distinct"

    '[:find ?n :with ?e ?f ?e :where [?e ?f ?n]]
    "Vars used in :with should be distinct"

    '[:find ?x :where [$1 ?x]]
    "Where uses unknown source vars: [$1]"

    '[:find ?x :in $1 :where [$2 ?x]]
    "Where uses unknown source vars: [$2]"

    '[:find ?e :where (rule ?e)]
    "Missing rules var '%' in :in"
    ))

(deftest clauses
  (are [form res] (= (set (dp/parse-rules form)) res)
    '[[(rule ?x)
       [?x :name _]]]
    #{(dp/->Rule
        (dp/->PlainSymbol 'rule)
        [ (dp/->RuleBranch
            (dp/->RuleVars nil [(dp/->Variable '?x)])
            [(dp/->Pattern
               (dp/->DefaultSrc)
               [(dp/->Variable '?x) (dp/->Constant :name) (dp/->Placeholder)])]) ])})
  (is (thrown-with-msg? ExceptionInfo #"Reference to the unknown variable"
        (dp/parse-rules '[[(rule ?x) [?x :name ?y]]]))))

(deftest rule-vars
  (are [form res] (= (set (dp/parse-rules form)) res)
    '[[(rule [?x] ?y)
       [_]]]
    #{(dp/->Rule
        (dp/->PlainSymbol 'rule)
        [ (dp/->RuleBranch
            (dp/->RuleVars [(dp/->Variable '?x)] [(dp/->Variable '?y)])
            [(dp/->Pattern (dp/->DefaultSrc) [(dp/->Placeholder)])]) ])}

    '[[(rule [?x ?y] ?a ?b)
       [_]]]
    #{(dp/->Rule
        (dp/->PlainSymbol 'rule)

        [ (dp/->RuleBranch
           (dp/->RuleVars [(dp/->Variable '?x) (dp/->Variable '?y)]
                         [(dp/->Variable '?a) (dp/->Variable '?b)])
           [(dp/->Pattern (dp/->DefaultSrc) [(dp/->Placeholder)])]) ])}

    '[[(rule [?x])
       [_]]]
    #{(dp/->Rule
        (dp/->PlainSymbol 'rule)
        [ (dp/->RuleBranch
            (dp/->RuleVars [(dp/->Variable '?x)] nil)
            [(dp/->Pattern (dp/->DefaultSrc) [(dp/->Placeholder)])]) ])})

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse rule-vars"
        (dp/parse-rules '[[(rule) [_]]])))

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse rule-vars"
      (dp/parse-rules '[[(rule []) [_]]])))

  (is (thrown-with-msg? ExceptionInfo #"Rule variables should be distinct"
        (dp/parse-rules '[[(rule ?x ?y ?x) [_]]])))

  (is (thrown-with-msg? ExceptionInfo #"Rule variables should be distinct"
        (dp/parse-rules '[[(rule [?x ?y] ?z ?x) [_]]])))
)

(deftest branches
  (are [form res] (= (set (dp/parse-rules form)) res)
    '[[(rule ?x)
       [:a]
       [:b]]
      [(rule ?x)
       [:c]]]
    #{(dp/->Rule
        (dp/->PlainSymbol 'rule)
        [ (dp/->RuleBranch
            (dp/->RuleVars nil [(dp/->Variable '?x)])
            [(dp/->Pattern (dp/->DefaultSrc) [(dp/->Constant :a)])
             (dp/->Pattern (dp/->DefaultSrc) [(dp/->Constant :b)])])
          (dp/->RuleBranch
            (dp/->RuleVars nil [(dp/->Variable '?x)])
            [(dp/->Pattern (dp/->DefaultSrc) [(dp/->Constant :c)])]) ])}

    '[[(rule ?x)
       [:a]
       [:b]]
      [(other ?x)
       [:c]]]
    #{(dp/->Rule
        (dp/->PlainSymbol 'rule)
        [ (dp/->RuleBranch
            (dp/->RuleVars nil [(dp/->Variable '?x)])
            [(dp/->Pattern (dp/->DefaultSrc) [(dp/->Constant :a)])
             (dp/->Pattern (dp/->DefaultSrc) [(dp/->Constant :b)])]) ])
      (dp/->Rule
        (dp/->PlainSymbol 'other)
        [ (dp/->RuleBranch
            (dp/->RuleVars nil [(dp/->Variable '?x)])
            [(dp/->Pattern (dp/->DefaultSrc) [(dp/->Constant :c)])]) ])}
  )

  (is (thrown-with-msg? ExceptionInfo #"Rule branch should have clauses"
        (dp/parse-rules '[[(rule ?x)]])))

  (is (thrown-with-msg? ExceptionInfo #"Arity mismatch"
        (dp/parse-rules '[[(rule ?x) [_]]
                           [(rule ?x ?y) [_]]])))

  (is (thrown-with-msg? ExceptionInfo #"Arity mismatch"
        (dp/parse-rules '[[(rule ?x) [_]]
                           [(rule [?x]) [_]]])))
)

(deftest pattern
  (are [clause pattern] (= (dp/parse-clause clause) pattern)
    '[?e ?a ?v]
    (dp/->Pattern (dp/->DefaultSrc) [(dp/->Variable '?e) (dp/->Variable '?a) (dp/->Variable '?v)])

    '[_ ?a _ _]
    (dp/->Pattern (dp/->DefaultSrc) [(dp/->Placeholder) (dp/->Variable '?a) (dp/->Placeholder) (dp/->Placeholder)])

    '[$x _ ?a _ _]
    (dp/->Pattern (dp/->SrcVar '$x) [(dp/->Placeholder) (dp/->Variable '?a) (dp/->Placeholder) (dp/->Placeholder)])

    '[$x _ :name ?v]
    (dp/->Pattern (dp/->SrcVar '$x) [(dp/->Placeholder) (dp/->Constant :name) (dp/->Variable '?v)]))

    (is (thrown-with-msg? ExceptionInfo #"Pattern could not be empty"
                          (dp/parse-clause '[])))
)

(deftest test-pred
  (are [clause res] (= (dp/parse-clause clause) res)
    '[(pred ?a 1)]
    (dp/->Predicate (dp/->PlainSymbol 'pred) [(dp/->Variable '?a) (dp/->Constant 1)])

    '[(pred)]
    (dp/->Predicate (dp/->PlainSymbol 'pred) [])

    '[(?custom-pred ?a)]
    (dp/->Predicate (dp/->Variable '?custom-pred) [(dp/->Variable '?a)])
))

(deftest test-fn
  (are [clause res] (= (dp/parse-clause clause) res)
    '[(fn ?a 1) ?x]
    (dp/->Function (dp/->PlainSymbol 'fn) [(dp/->Variable '?a) (dp/->Constant 1)] (dp/->BindScalar (dp/->Variable '?x)))

    '[(fn) ?x]
    (dp/->Function (dp/->PlainSymbol 'fn) [] (dp/->BindScalar (dp/->Variable '?x)))

    '[(?custom-fn) ?x]
    (dp/->Function (dp/->Variable '?custom-fn) [] (dp/->BindScalar (dp/->Variable '?x)))

    '[(?custom-fn ?arg) ?x]
    (dp/->Function (dp/->Variable '?custom-fn) [(dp/->Variable '?arg)] (dp/->BindScalar (dp/->Variable '?x)))))

(deftest rule-expr
  (are [clause res] (= (dp/parse-clause clause) res)
    '(friends ?x ?y)
    (dp/->RuleExpr (dp/->DefaultSrc) (dp/->PlainSymbol 'friends) [(dp/->Variable '?x) (dp/->Variable '?y)])

    '(friends "Ivan" _)
    (dp/->RuleExpr (dp/->DefaultSrc) (dp/->PlainSymbol 'friends) [(dp/->Constant "Ivan") (dp/->Placeholder)])

    '($1 friends ?x ?y)
    (dp/->RuleExpr (dp/->SrcVar '$1) (dp/->PlainSymbol 'friends) [(dp/->Variable '?x) (dp/->Variable '?y)]))

  (is (thrown-with-msg? ExceptionInfo #"rule-expr requires at least one argument"
        (dp/parse-clause '(friends))))

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse rule-expr arguments"
        (dp/parse-clause '(friends something)))))

(deftest not-clause
  (are [clause res] (= (dp/parse-clause clause) res)
    '(not [?e :follows ?x])
    (dp/->Not
      (dp/->DefaultSrc)
      [(dp/->Variable '?e) (dp/->Variable '?x)]
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)]) ])

    '(not
       [?e :follows ?x]
       [?x _ ?y])
    (dp/->Not
      (dp/->DefaultSrc)
      [(dp/->Variable '?e) (dp/->Variable '?x) (dp/->Variable '?y)]
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)])
        (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?x) (dp/->Placeholder) (dp/->Variable '?y)])])

    '($1 not [?x])
    (dp/->Not
      (dp/->SrcVar '$1)
      [(dp/->Variable '?x)]
      [ (dp/->Pattern (dp/->DefaultSrc) [(dp/->Variable '?x)]) ])

    '(not-join [?e ?y]
       [?e :follows ?x]
       [?x _ ?y])
    (dp/->Not
      (dp/->DefaultSrc)
      [(dp/->Variable '?e) (dp/->Variable '?y)]
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)])
        (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?x) (dp/->Placeholder) (dp/->Variable '?y)])])

    '($1 not-join [?e] [?e :follows ?x])
    (dp/->Not
      (dp/->SrcVar '$1)
      [(dp/->Variable '?e)]
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)]) ])
  )

  (is (thrown-with-msg? ExceptionInfo #"Join variable not declared inside clauses: \[\?x\]"
        (dp/parse-clause '(not-join [?x] [?y]))))

  (is (thrown-with-msg? ExceptionInfo #"Join variables should not be empty"
        (dp/parse-clause '(not-join [] [?y]))))

  (is (thrown-with-msg? ExceptionInfo #"Join variables should not be empty"
        (dp/parse-clause '(not [_]))))

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse 'not-join' clause"
        (dp/parse-clause '(not-join [?x]))))

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse 'not' clause"
      (dp/parse-clause '(not))))

  (is (thrown-with-msg? ExceptionInfo #"Join variable not declared inside clauses: \[\?y\]"
        (dp/parse-clause '(not-join [?y]
                             (not-join [?x]
                               [?x :follows ?y])))))
)

(deftest or-clause
  (are [clause res] (= (dp/parse-clause clause) res)
    '(or [?e :follows ?x])
    (dp/->Or
      (dp/->DefaultSrc)
      (dp/->RuleVars nil [(dp/->Variable '?e) (dp/->Variable '?x)])
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)]) ])

    '(or
       [?e :follows ?x]
       [?e :friend ?x])
    (dp/->Or
      (dp/->DefaultSrc)
      (dp/->RuleVars nil [(dp/->Variable '?e) (dp/->Variable '?x)])
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)])
        (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :friend) (dp/->Variable '?x)])])

    '(or
       [?e :follows ?x]
       (and
         [?e :friend ?x]
         [?x :friend ?e]))
    (dp/->Or
      (dp/->DefaultSrc)
      (dp/->RuleVars nil [(dp/->Variable '?e) (dp/->Variable '?x)])
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)])
        (dp/->And
          [(dp/->Pattern
             (dp/->DefaultSrc)
             [(dp/->Variable '?e) (dp/->Constant :friend) (dp/->Variable '?x)])
           (dp/->Pattern
             (dp/->DefaultSrc)
             [(dp/->Variable '?x) (dp/->Constant :friend) (dp/->Variable '?e)])]) ])

    '($1 or [?x])
    (dp/->Or
      (dp/->SrcVar '$1)
      (dp/->RuleVars nil [(dp/->Variable '?x)])
      [ (dp/->Pattern (dp/->DefaultSrc) [(dp/->Variable '?x)]) ])

    '(or-join [?e]
       [?e :follows ?x]
       [?e :friend ?y])
    (dp/->Or
      (dp/->DefaultSrc)
      (dp/->RuleVars nil [(dp/->Variable '?e)])
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)])
        (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :friend) (dp/->Variable '?y)])])

    '(or-join [[?e]]
       (and [?e :follows ?x]
            [?e :friend ?y]))
    (dp/->Or
      (dp/->DefaultSrc)
      (dp/->RuleVars [(dp/->Variable '?e)] nil)
      [ (dp/->And
          [(dp/->Pattern
             (dp/->DefaultSrc)
             [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)])
           (dp/->Pattern
             (dp/->DefaultSrc)
             [(dp/->Variable '?e) (dp/->Constant :friend) (dp/->Variable '?y)])]) ])

    '($1 or-join [[?e] ?x]
         [?e :follows ?x])
    (dp/->Or
      (dp/->SrcVar '$1)
      (dp/->RuleVars [(dp/->Variable '?e)] [(dp/->Variable '?x)])
      [ (dp/->Pattern
          (dp/->DefaultSrc)
          [(dp/->Variable '?e) (dp/->Constant :follows) (dp/->Variable '?x)]) ])
  )

  (is (thrown-with-msg? ExceptionInfo #"Join variable not declared inside clauses: \[\?x\]"
        (dp/parse-clause '(or-join [?x] [?y]))))

  (is (thrown-with-msg? ExceptionInfo #"Join variable not declared inside clauses: \[\?y\]"
        (dp/parse-clause '(or [?x] [?x ?y]))))

  (is (thrown-with-msg? ExceptionInfo #"Join variable not declared inside clauses: \[\?y\]"
        (dp/parse-clause '(or [?x] [?y]))))

  (is (thrown-with-msg? ExceptionInfo #"Join variable not declared inside clauses: \[\?x\]"
        (dp/parse-clause '(or-join [?x ?y] [?x ?y] [?y]))))

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse rule-vars"
        (dp/parse-clause '(or-join [] [?y]))))

  (is (thrown-with-msg? ExceptionInfo #"Join variables should not be empty"
        (dp/parse-clause '(or [_]))))

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse 'or-join' clause"
        (dp/parse-clause '(or-join [?x]))))

  (is (thrown-with-msg? ExceptionInfo #"Cannot parse 'or' clause"
      (dp/parse-clause '(or))))

  (is (thrown-with-msg? ExceptionInfo #"Join variable not declared inside clauses: \[\?y\]"
        (dp/parse-clause '(or-join [?y]
                             (or-join [?x]
                               [?x :follows ?y])))))
)
