(ns datalog.parser-test
  (:require #?(:cljs [cljs.test :refer-macros [are deftest is]]
               :clj  [clojure.test :refer [are deftest is]])
            [datalog.parser :as parser])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(deftest validation
  (are [q result] (= result (parser/parse q))
    '[:find ?e
      :in $ ?fname ?lname
      :keys foo
      :where [?e :user/firstName ?fname]
      [?e :user/lastName ?lname]]
    '#datalog.parser.type.Query
      {:qfind #datalog.parser.type.FindRel {:elements [#datalog.parser.type.Variable
                                                        {:symbol ?e}]},
       :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar
                                                         {:symbol $}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?fname}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?lname}}],
       :qlimit nil,
       :qoffset nil,
       :qhaving nil,
       :qorder nil,
       :qtimeout nil,
       :qreturnmaps #datalog.parser.type.ReturnMaps
                     {:mapping-keys (#datalog.parser.type.MappingKey {:mapping-key
                                                                      foo}),
                      :mapping-type :keys},
       :qwhere [#datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/firstName}
                            #datalog.parser.type.Variable {:symbol ?fname}],
                  :source #datalog.parser.type.DefaultSrc {}}
                #datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/lastName}
                            #datalog.parser.type.Variable {:symbol ?lname}],
                  :source #datalog.parser.type.DefaultSrc {}}],
       :qwith nil}

    '[:find ?e
      :in $ ?fname ?lname
      :strs foo
      :where [?e :user/firstName ?fname]
      [?e :user/lastName ?lname]]
    '#datalog.parser.type.Query
      {:qfind #datalog.parser.type.FindRel {:elements [#datalog.parser.type.Variable
                                                        {:symbol ?e}]},
       :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar
                                                         {:symbol $}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?fname}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?lname}}],
       :qlimit nil,
       :qoffset nil,
       :qhaving nil,
       :qorder nil,
       :qtimeout nil,
       :qreturnmaps #datalog.parser.type.ReturnMaps
                     {:mapping-keys (#datalog.parser.type.MappingKey {:mapping-key
                                                                      foo}),
                      :mapping-type :strs},
       :qwhere [#datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/firstName}
                            #datalog.parser.type.Variable {:symbol ?fname}],
                  :source #datalog.parser.type.DefaultSrc {}}
                #datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/lastName}
                            #datalog.parser.type.Variable {:symbol ?lname}],
                  :source #datalog.parser.type.DefaultSrc {}}],
       :qwith nil}

    '[:find ?e
      :in $ ?fname ?lname
      :syms foo
      :where [?e :user/firstName ?fname]
      [?e :user/lastName ?lname]]
    '#datalog.parser.type.Query
      {:qfind #datalog.parser.type.FindRel {:elements [#datalog.parser.type.Variable
                                                        {:symbol ?e}]},
       :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar
                                                         {:symbol $}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?fname}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?lname}}],
       :qlimit nil,
       :qoffset nil,
       :qhaving nil,
       :qorder nil,
       :qtimeout nil,
       :qreturnmaps #datalog.parser.type.ReturnMaps
                     {:mapping-keys (#datalog.parser.type.MappingKey {:mapping-key
                                                                      foo}),
                      :mapping-type :syms},
       :qwhere [#datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/firstName}
                            #datalog.parser.type.Variable {:symbol ?fname}],
                  :source #datalog.parser.type.DefaultSrc {}}
                #datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/lastName}
                            #datalog.parser.type.Variable {:symbol ?lname}],
                  :source #datalog.parser.type.DefaultSrc {}}],
       :qwith nil}

    '{:find [?e]
      :in [$ ?fname ?lname]
      :keys [foo]
      :where [[?e :user/firstName ?fname]
              [?e :user/lastName ?lname]]}
    '#datalog.parser.type.Query
      {:qfind #datalog.parser.type.FindRel {:elements [#datalog.parser.type.Variable
                                                        {:symbol ?e}]},
       :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar
                                                         {:symbol $}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?fname}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?lname}}],
       :qlimit nil,
       :qoffset nil,
       :qhaving nil,
       :qorder nil,
       :qtimeout nil,
       :qreturnmaps #datalog.parser.type.ReturnMaps
                     {:mapping-keys (#datalog.parser.type.MappingKey {:mapping-key
                                                                      foo}),
                      :mapping-type :keys},
       :qwhere [#datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/firstName}
                            #datalog.parser.type.Variable {:symbol ?fname}],
                  :source #datalog.parser.type.DefaultSrc {}}
                #datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/lastName}
                            #datalog.parser.type.Variable {:symbol ?lname}],
                  :source #datalog.parser.type.DefaultSrc {}}],
       :qwith nil}

    '{:find [[?e ?fname]]
      :keys [foo]
      :in [$ ?fname ?lname]
      :where [[?e :user/firstName ?fname]
              [?e :user/lastName ?lname]]}
    '#datalog.parser.type.Query
      {:qfind #datalog.parser.type.FindTuple
               {:elements [#datalog.parser.type.Variable {:symbol ?e}
                           #datalog.parser.type.Variable {:symbol ?fname}]},
       :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar
                                                         {:symbol $}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?fname}}
             #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                         {:symbol ?lname}}],
       :qlimit nil,
       :qoffset nil,
       :qhaving nil,
       :qorder nil,
       :qtimeout nil,
       :qreturnmaps #datalog.parser.type.ReturnMaps
                     {:mapping-keys (#datalog.parser.type.MappingKey {:mapping-key
                                                                      foo}),
                      :mapping-type :keys},
       :qwhere [#datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/firstName}
                            #datalog.parser.type.Variable {:symbol ?fname}],
                  :source #datalog.parser.type.DefaultSrc {}}
                #datalog.parser.type.Pattern
                 {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                            #datalog.parser.type.Constant {:value :user/lastName}
                            #datalog.parser.type.Variable {:symbol ?lname}],
                  :source #datalog.parser.type.DefaultSrc {}}],
       :qwith nil}

    '{:find [?e]
      :where [[?e :user/age 30]]}
    '#datalog.parser.type.Query{:qfind #datalog.parser.type.FindRel {:elements [#datalog.parser.type.Variable {:symbol ?e}]},
                                :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar {:symbol $}}]
                                :qlimit nil,
                                :qoffset nil,
                                :qhaving nil,
                                :qorder nil,
                                :qtimeout nil,
                                :qreturnmaps nil,
                                :qwhere [#datalog.parser.type.Pattern
                                          {:source #datalog.parser.type.DefaultSrc {}
                                           :pattern [#datalog.parser.type.Variable {:symbol ?e}
                                                     #datalog.parser.type.Constant {:value :user/age}
                                                     #datalog.parser.type.Constant {:value 30}]}]
                                :qwith nil}

    '{:find [?e]
      :where [[?e :user/registered? true]]}
    '#datalog.parser.type.Query{:qfind #datalog.parser.type.FindRel {:elements [#datalog.parser.type.Variable {:symbol ?e}]},
                                :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar {:symbol $}}],
                                :qwhere [#datalog.parser.type.Pattern
                                          {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                                                     #datalog.parser.type.Constant {:value :user/registered?}
                                                     #datalog.parser.type.Constant {:value true}],
                                           :source #datalog.parser.type.DefaultSrc {}}],
                                :qwith nil,
                                :qlimit nil,
                                :qoffset nil
                                :qhaving nil,
                                :qorder nil,
                                :qtimeout nil
                                :qreturnmaps nil}

    '{:find [?e]
      :where [[?e :user/name "Victor"]]}
    '#datalog.parser.type.Query{:qfind #datalog.parser.type.FindRel {:elements [#datalog.parser.type.Variable {:symbol ?e}]},
                                :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar {:symbol $}}],
                                :qwhere [#datalog.parser.type.Pattern
                                          {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                                                     #datalog.parser.type.Constant {:value :user/name}
                                                     #datalog.parser.type.Constant {:value "Victor"}],
                                           :source #datalog.parser.type.DefaultSrc {}}],
                                :qwith nil,
                                :qlimit nil,
                                :qoffset nil,
                                :qhaving nil,
                                :qorder nil,
                                :qtimeout nil,
                                :qreturnmaps nil})

  (is (= (parser/parse '[:find ?e
                         :in $ ?fname ?lname
                         :keys :foo
                         :where [?e :user/firstName ?fname]
                         [?e :user/lastName ?lname]])
         '#datalog.parser.type.Query
           {:qfind #datalog.parser.type.FindRel {:elements [#datalog.parser.type.Variable
                                                             {:symbol ?e}]},
            :qin [#datalog.parser.type.BindScalar {:variable #datalog.parser.type.SrcVar
                                                              {:symbol $}}
                  #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                              {:symbol ?fname}}
                  #datalog.parser.type.BindScalar {:variable #datalog.parser.type.Variable
                                                              {:symbol ?lname}}],
            :qlimit nil,
            :qoffset nil,
            :qhaving nil,
            :qorder nil,
            :qtimeout nil,
            :qreturnmaps #datalog.parser.type.ReturnMaps
                          {:mapping-keys (#datalog.parser.type.MappingKey {:mapping-key
                                                                           :foo}),
                           :mapping-type :keys},
            :qwhere [#datalog.parser.type.Pattern
                      {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                                 #datalog.parser.type.Constant {:value :user/firstName}
                                 #datalog.parser.type.Variable {:symbol ?fname}],
                       :source #datalog.parser.type.DefaultSrc {}}
                     #datalog.parser.type.Pattern
                      {:pattern [#datalog.parser.type.Variable {:symbol ?e}
                                 #datalog.parser.type.Constant {:value :user/lastName}
                                 #datalog.parser.type.Variable {:symbol ?lname}],
                       :source #datalog.parser.type.DefaultSrc {}}],
            :qwith nil})))

(deftest order-by
  (are [q result] (= result (:qorder (parser/parse q)))
    '[:find ?e ?age :where [?e :age ?age]]
    nil

    '[:find ?e ?age :where [?e :age ?age] :order-by ?age]
    '[#datalog.parser.type.Order {:element #datalog.parser.type.Variable {:symbol ?age}
                                  :direction :asc}]

    '[:find ?e ?age :where [?e :age ?age] :order-by [?age :desc]]
    '[#datalog.parser.type.Order {:element #datalog.parser.type.Variable {:symbol ?age}
                                  :direction :desc}]

    '[:find ?e ?age :where [?e :age ?age] :order-by [?age :desc ?e]]
    '[#datalog.parser.type.Order {:element #datalog.parser.type.Variable {:symbol ?age}
                                  :direction :desc}
      #datalog.parser.type.Order {:element #datalog.parser.type.Variable {:symbol ?e}
                                  :direction :asc}]

    '[:find ?e ?age :where [?e :age ?age] :order-by [1 :desc]]
    '[#datalog.parser.type.Order {:element #datalog.parser.type.Constant {:value 1}
                                  :direction :desc}]

    ;; map queries pass the spec through as it was written
    '{:find [?e ?age] :where [[?e :age ?age]] :order-by ?age}
    '[#datalog.parser.type.Order {:element #datalog.parser.type.Variable {:symbol ?age}
                                  :direction :asc}]

    '{:find [?e ?age] :where [[?e :age ?age]] :order-by [?age :desc ?e :asc]}
    '[#datalog.parser.type.Order {:element #datalog.parser.type.Variable {:symbol ?age}
                                  :direction :desc}
      #datalog.parser.type.Order {:element #datalog.parser.type.Variable {:symbol ?e}
                                  :direction :asc}]))

(deftest having
  (are [q result] (= result (:qhaving (parser/parse q)))
    '[:find ?e (count ?x) :where [?e :friend ?x]]
    nil

    '[:find ?e (count ?x) :where [?e :friend ?x] :having [(> (count ?x) 3)]]
    '[#datalog.parser.type.HavingPred
       {:fn #datalog.parser.type.PlainSymbol {:symbol >}
        :args [#datalog.parser.type.Aggregate
                {:fn #datalog.parser.type.PlainSymbol {:symbol count}
                 :args [#datalog.parser.type.Variable {:symbol ?x}]}
               #datalog.parser.type.Constant {:value 3}]}]

    '{:find [?e (count ?x)] :where [[?e :friend ?x]] :having [(> (count ?x) 3)]}
    '[#datalog.parser.type.HavingPred
       {:fn #datalog.parser.type.PlainSymbol {:symbol >}
        :args [#datalog.parser.type.Aggregate
                {:fn #datalog.parser.type.PlainSymbol {:symbol count}
                 :args [#datalog.parser.type.Variable {:symbol ?x}]}
               #datalog.parser.type.Constant {:value 3}]}]))

(deftest pagination
  (are [q result] (= result ((juxt :qlimit :qoffset :qtimeout) (parser/parse q)))
    '[:find ?e :where [?e :age 30] :limit 5 :offset 10 :timeout 1000]
    [5 10 1000]

    '{:find [?e] :where [[?e :age 30]] :limit 5 :offset 10 :timeout 1000}
    [5 10 1000]))

(deftest validation-fails
  (are [q msg] (thrown-with-msg? ExceptionInfo msg (parser/parse q))
    '[:find ?e :where [?x]]
    #"Query for unknown vars: \[\?e\]"

    '[:find ?e :with ?f :where [?e]]
    #"Query for unknown vars: \[\?f\]"

    '[:find ?e ?x ?t :in ?x :where [?e]]
    #"Query for unknown vars: \[\?t\]"

    '[:find ?x ?e :with ?y ?e :where [?x ?e ?y]]
    #":find and :with should not use same variables: \[\?e\]"

    '[:find ?e :in $ $ ?x :where [?e]]
    #"Vars used in :in should be distinct"

    '[:find ?e :in ?x $ ?x :where [?e]]
    #"Vars used in :in should be distinct"

    '[:find ?e :in $ % ?x % :where [?e]]
    #"Vars used in :in should be distinct"

    '[:find ?n :with ?e ?f ?e :where [?e ?f ?n]]
    #"Vars used in :with should be distinct"

    '[:find ?x :where [$1 ?x]]
    #"Where uses unknown source vars: \[\$1\]"

    '[:find ?x :in $1 :where [$2 ?x]]
    #"Where uses unknown source vars: \[\$2\]"

    '[:find ?e :where (rule ?e)]
    #"Missing rules var '%' in :in"

    '[:find ?e :where [?e] :limit [42]]
    #"Cannot parse :limit, expected integer"

    '[:find ?e :where [?e] :offset [666]]
    #"Cannot parse :offset, expected integer"

    '[:find ?e :keys foo bar :where [?e] :offset 666]
    #"Count of :keys/:strs/:syms must match count of :find"

    '[:find ?e ?f :keys foo :where [?e ?f] :offset 666]
    #"Count of :keys/:strs/:syms must match count of :find"

    '[:find [?e ?f] :keys foo bar :where [?e ?f] :offset 666]
    #"Count of :keys/:strs/:syms must match count of :find"

    '[:find ?e :strs '(foo bar) :keys '("foo" "bar") :where [?e] :offset 666]
    #"Only one of these three options is allowed: :keys :strs :syms"

    '[:find ?e ?age :where [?e :age ?age] :order-by [:desc ?age]]
    #"Cannot parse :order-by"

    '[:find ?e ?age :where [?e :age ?age] :order-by ["age"]]
    #"Cannot parse :order-by"

    '[:find ?e :where [?e :age ?age] :order-by ?age]
    #":order-by uses vars that are not in :find: \[\?age\]"

    '[:find ?e ?age :where [?e :age ?age] :order-by [?age :desc ?age]]
    #"Vars used in :order-by should be distinct"

    '[:find ?e ?age :where [?e :age ?age] :order-by [2]]
    #":order-by column index out of bounds: \[2\]"

    '[:find ?e (count ?x) :where [?e :friend ?x] :having [?x]]
    #"Cannot parse :having"

    '[:find ?e :where [?e :age 30] :timeout "soon"]
    #"Cannot parse :timeout, expected integer"))

