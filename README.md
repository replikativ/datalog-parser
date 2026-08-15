# datalog-parser

<p align="center">
<a href="https://clojurians.slack.com/archives/CB7GJAN0L"><img src="https://img.shields.io/badge/clojurians%20slack-join%20channel-blueviolet"/></a>
<a href="https://clojars.org/org.replikativ/datalog-parser"> <img src="https://img.shields.io/clojars/v/org.replikativ/datalog-parser.svg" /></a>
<a href="https://circleci.com/gh/replikativ/datalog-parser"><img src="https://circleci.com/gh/replikativ/datalog-parser.svg?style=shield"/></a>
<a href="https://versions.deps.co/replikativ/datalog-parser" title="Dependencies Status"><img src="https://versions.deps.co/replikativ/datalog-parser/status.svg" /></a>
</p>

A Datalog parser. This parser is used by [Datahike](https://github.com/replikativ/datahike) and follows the Datalog dialect of [Datomic](https://www.datomic.com/).

Note: This repository has been moved from the [lambdaforge organization](https://github.com/replikativ) to [replikativ](https://github.com/replikativ). So, you will find older releases of the parser at the [lambdaforge clojars page](https://clojars.org/io.lambdaforge/datalog-parser).

## Usage
Add the current release of `org.replikativ/datalog-parser` to your `deps.edn`:

```clojure
org.replikativ/datalog-parser {:mvn/version "0.2.XX"}
```

### Parsing

```clojure
(require '[datalog.parser :as parser])

(parser/parse '[:find ?x :in $ ?y :where [?x :z ?y]])

;;=> (namespaces omitted for brevity)
;; #Query{:qfind  #FindRel{:elements [#Variable{:symbol ?x}]}
;;        :qwith  nil
;;        :qin    [#BindScalar{:variable #SrcVar{:symbol $}}
;;                 #BindScalar{:variable #Variable{:symbol ?y}}]
;;        :qwhere [#Pattern{:source #DefaultSrc{}
;;                          :pattern [#Variable{:symbol ?x}
;;                                    #Constant{:value  :z}
;;                                    #Variable{:symbol ?y}]}]}
```

Besides the Datomic dialect the parser understands the clauses that
[Datahike](https://github.com/replikativ/datahike) and
[Datalevin](https://github.com/juji-io/datalevin) add on top of it: `:limit`,
`:offset` and `:order-by` for pagination and ordering, `:having` for predicates
over aggregates and `:timeout`. They are parsed in both the vector and the map
syntax:

```clojure
(parser/parse '[:find ?e ?age
                :where [?e :age ?age]
                :order-by [?age :desc]
                :limit 10])

;;=> #Query{... :qorder  [#Order{:element #Variable{:symbol ?age}, :direction :desc}]
;;              :qlimit  10}
```

An `:order-by` element is either a variable of the `:find` spec or an index into
it, optionally followed by `:asc` or `:desc`, which defaults to `:asc`. A
`:having` predicate reads like a `:where` predicate, but over the aggregates of
the `:find` spec, e.g. `:having [(> (count ?x) 3)]`.

### Unparsing

Convert parsed query records back into the query DSL. This enables programmatic
query modification:

```clojure
(require '[datalog.unparser :as unparser])

;; Round-trip: parse, modify, unparse
(let [parsed (parser/parse '[:find ?x :in $ :where [?x :name "Ivan"]])]
  (unparser/unparse parsed))
;;=> [:find ?x :in $ :where [?x :name "Ivan"]]

;; Rules round-trip
(let [rules (parser/parse-rules '[[(ancestor ?e1 ?e2) [?e1 :parent ?e2]]
                                  [(ancestor ?e1 ?e2) [?e1 :parent ?t]
                                   (ancestor ?t ?e2)]])]
  (unparser/unparse-rules rules))
;;=> [[(ancestor ?e1 ?e2) [?e1 :parent ?e2]]
;;    [(ancestor ?e1 ?e2) [?e1 :parent ?t] (ancestor ?t ?e2)]]
```

### Analysis

`datalog.analysis` answers questions about a parsed query that hold whatever
engine runs it. A clause like `[(> ?a 18)]` cannot run before something binds
`?a`, but the order the clauses are written in does not matter, since an engine
may reorder them. Whether *any* order can work is decidable from the parsed
query alone:

```clojure
(require '[datalog.analysis :as analysis])

;; written backwards, but an engine can run the pattern first
(analysis/unsatisfiable-clauses
  (parser/parse '[:find ?e :in $ :where [(> ?a 18)] [?e :age ?a]]))
;;=> []

;; nothing binds ?b, in any order
(analysis/unsatisfiable-clauses
  (parser/parse '[:find ?e :in $ :where [?e :age ?a] [(> ?b 18)]]))
;;=> [{:clause #Predicate{...}, :form [(> ?b 18)], :missing #{?b}}]
```

`assert-satisfiable` raises on the same condition and returns the query
otherwise, and `bindable-vars` gives the set of variables some ordering can
bind. The analysis is opt-in, `parse` does not run it, and it errs towards
saying nothing: a query is only reported when no ordering can work.

For more examples look at the [tests](test/datalog/parser_test.cljc).

## License

Copyright © 2020-2026 Christian Weilbach et al.

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 1.0.
