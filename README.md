# datalog-parser <a href="https://clojars.org/io.lambdaforge/datalog-parser"> <img src="https://img.shields.io/clojars/v/io.lambdaforge/datalog-parser.svg" /></a> 

Datahike's form parser.

## Usage
Add `[io.lambdaforge/datalog-parser "0.0.1"]` to your `project.clj`. Start a repl and run:

```Clojure
(require '[datalog-parser.core :as dp])

(def query '[:find ?x :in $ ?y :where [?x :z ?y]])

(dp/parse-query query)

;;=>
;;#datalog-parser.coreQuery{:qfind #datalog-parser.FindRel{:elements [#datalog-parser.Variable{:symbol ?x}]},
;;                       :qwith nil,
;;                       :qin [#datalog-parser.coreBindScalar{:variable #datalog-parser.SrcVar{:symbol $}}
;;                             #datalog-parser.coreBindScalar{:variable #datalog-parser.Variable{:symbol ?y}}],
;;                       :qwhere [#datalog-parser.corePattern{:source #datalog-parser.DefaultSrc{},
;;                                                         :pattern [#datalog-parser.coreVariable{:symbol ?x}
;;                                                                   #datalog-parser.coreConstant{:value :z}
;;                                                                   #datalog-parser.coreVariable{:symbol ?y}]}]}
```

For more examples look at the [tests](https://github.com/lambdaforge/datalog-parser/blob/master/test/datalog-parser_test.cljc).


## License

Copyright © 2019 lambdaforge UG (haftungsbeschränkt), Nikita Prokopov

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
