# datalog-parser <a href="https://clojars.org/io.lambdaforge/datalog-parser"> <img src="https://img.shields.io/clojars/v/io.lambdaforge/datalog-parser.svg" /></a>

A Datalog parser.

## Usage
Add `[io.lambdaforge/datalog-parser "0.1.1"]` to your `project.clj`. Start a repl and run:

```Clojure
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

For more examples look at the [tests](test/datalog/parser_test.cljc).


## License

Copyright © 2019 lambdaforge UG (haftungsbeschränkt), Nikita Prokopov

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 1.0.
