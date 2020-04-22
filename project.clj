(defproject io.lambdaforge/datalog-parser "0.1.2"
  :description  "Datalog parser."
  :url          "https://github.com/lambdaforge/datalog-parser"
  :license      {:name "EPL 1.0"
                 :url "https://www.eclipse.org/org/documents/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :repositories [["clojars" {:url "https://clojars.org/repo"
                             :sign-releases false}]]
  :repl-options {:init-ns datalog.parser})
