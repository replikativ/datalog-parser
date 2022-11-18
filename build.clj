(ns build
  (:refer-clojure :exclude [test compile])
  (:require [clojure.tools.build.api :as b]
            [borkdude.gh-release-artifact :as gh]
            [org.corfield.build :as bb])
  (:import (clojure.lang ExceptionInfo)))

(def lib 'io.lambdaforge/datalog-parser)
(def version (format "0.2.%s" (b/git-count-revs nil)))
(def current-commit (gh/current-commit))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean
  [_]
  (b/delete {:path "target"}))

(defn jar
  [opts]
  (-> opts
      (assoc :class-dir class-dir
             :src-pom "./template/pom.xml"
             :lib lib
             :version version
             :basis basis
             :jar-file jar-file
             :src-dirs ["src"])
      bb/jar))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/clean)
      (bb/jar)))

(defn install "Install the JAR locally." [opts]
  (-> opts
      jar
      bb/install))

(defn deploy "Deploy the JAR to Clojars." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/deploy)))

(defn fib [a b]
  (lazy-seq (cons a (fib b (+ a b)))))

(defn retry-with-fib-backoff [retries exec-fn test-fn]
  (loop [idle-times (take retries (fib 1 2))]
    (let [result (exec-fn)]
      (if (test-fn result)
        (when-let [sleep-ms (first idle-times)]
          (println "Returned: " result)
          (println "Retrying with remaining back-off times (in s): " idle-times)
          (Thread/sleep (* 1000 sleep-ms))
          (recur (rest idle-times)))
        result))))

(defn try-release []
  (try (gh/overwrite-asset {:org "lambdaforge"
                            :repo (name lib)
                            :tag version
                            :commit current-commit
                            :file jar-file
                            :content-type "application/java-archive"})
       (catch ExceptionInfo e
         (assoc (ex-data e) :failure? true))))

(defn release
  [_]
  (-> (retry-with-fib-backoff 10 try-release :failure?)
      :url
      println))

