(defproject io.lambdaforge/datalog-parser "0.0.1"
  :description "Parser for datalog forms"
  :url "https://github.com/lambdaforge/datalog-parser"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [persistent-sorted-set "0.1.1"]]
  :repl-options {:init-ns datalog-parser.core})
