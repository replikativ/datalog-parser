(ns datalog.parser-perf
  (:require [clojure.test :refer [deftest]]
            [datalog.parser :as parser]
            [datalog.parser.test.util]
            [criterium.core :as cc]
            [clj-async-profiler.core :as prof]))

(def q '[:find ?e
         :in $ ?fname ?lname
         :keys foo
         :where [?e :user/firstName ?fname]
         [?e :user/lastName ?lname]])

(deftest parse
  ;;; Baseline
  ;;; Execution time mean : 1.787494 ms
  ;;; Extend based:
  ;;; Execution time mean : 59.656711 µs ~ 30x faster
  (cc/bench (parser/parse q)))

(comment
  (prof/serve-files 7777))

(def reps 1e6)

(deftest profile
  (prof/profile
   (time
    (dotimes [_ reps]
      (parser/parse q)))))




