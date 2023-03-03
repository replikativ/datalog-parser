(ns datalog.parser.pull-test
  (:require [datalog.parser.pull :as dpp]
            #?(:cljs [cljs.test    :as t :refer-macros [is deftest testing]]
               :clj  [clojure.test :as t :refer        [is deftest testing]])))

#?(:cljs
   (def Throwable js/Error))

(def wildcard (dpp/->PullSpec true {}))

(deftest parse-pattern-test
  (testing "simple attribute"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo}})
           (dpp/parse-pull '[:foo]))))
  (testing "namespaced attribute"
    (is (= (dpp/->PullSpec false {:db/id   {:attr :db/id}
                                  :foo/bar {:attr :foo/bar}})
           (dpp/parse-pull '[:db/id :foo/bar]))))
  (testing "limit"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :limit 1}})
           (dpp/parse-pull '[(:foo :limit 1)]))))
  (testing "default"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :default "bar"}})
           (dpp/parse-pull '[(:foo :default "bar")]))))
  (testing "as"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :as "bar"}})
           (dpp/parse-pull '[(:foo :as "bar")])))))

(deftest map-specs-test
  (testing "wildcard"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :subpattern wildcard}})
           (dpp/parse-pull '[{:foo [*]}]))))
  (testing "subpattern"
    (is (= (dpp/->PullSpec
             false
             {:foo {:attr :foo
                    :subpattern (dpp/->PullSpec
                                  false
                                  {:bar {:attr :bar}
                                   :me {:attr :me}})}})
           (dpp/parse-pull '[{:foo [:bar :me]}]))))
  (testing "recursion"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :recursion nil}})
           (dpp/parse-pull '[{:foo ...}]))))
  (testing "limit"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :limit 1 :subpattern wildcard}})
           (dpp/parse-pull '[{(:foo :limit 1) [*]}]))))
  (testing "default"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :default "bar" :subpattern wildcard}})
           (dpp/parse-pull '[{(:foo :default "bar") [*]}]))))
  (testing "as"
      (is (= (dpp/->PullSpec false {:foo {:attr :foo :as "bar" :subpattern wildcard}})
             (dpp/parse-pull '[{(:foo :as "bar") [*]}])))))

(deftest legacy-parse-pattern-test
  (testing "limit"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :limit 1}})
           (dpp/parse-pull '[(limit :foo 1)])))
    (is (thrown? Throwable (dpp/parse-pull '[(limit :foo "bar")]))))
  (testing "default"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :default "bar"}})
           (dpp/parse-pull '[(default :foo "bar")])))
    (is (thrown? Throwable (dpp/parse-pull '[(default 1 :bar)])))))

(deftest legacy-map-specs-test
  (testing "limit"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :limit 1 :subpattern wildcard}})
           (dpp/parse-pull '[{(limit :foo 1) [*]}])))
    (is (thrown? Throwable
                 (dpp/parse-pull '[{(limit :foo "bar") [*]}]))))
  (testing "default"
    (is (= (dpp/->PullSpec false {:foo {:attr :foo :default "bar" :subpattern wildcard}})
           (dpp/parse-pull '[{(default :foo "bar") [*]}])))
    (is (thrown? Throwable
                 (dpp/parse-pull '[{(default 1 :bar) [*]}])))))

