(defproject io.lambdaforge/datalog-parser "0.1.5-SNAPSHOT"
  :description  "Datalog parser."
  :url          "https://github.com/lambdaforge/datalog-parser"
  :license      {:name "EPL 1.0"
                 :url "https://www.eclipse.org/org/documents/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :repositories [["clojars" {:url "https://clojars.org/repo"
                             :sign-releases false}]]
  :profiles {:dev {:source-paths ["perf"]
                   :dependencies [[com.clojure-goes-fast/clj-async-profiler "0.4.1"]
                                  [criterium "0.4.5"]]}}
  :repl-options {:init-ns datalog.parser})
