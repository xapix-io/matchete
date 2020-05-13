(ns matchete.pattern-test
  (:require [matchete.pattern :as sut]
            #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is] :include-macros true])))

(deftest match?-test
  (testing "constant patterns"
    (is (sut/match? 1 1))
    (is (sut/match? true true))
    (is (sut/match? [1 2] [1 2]))
    (is (sut/match? {:x 1} {:x 1}))))
