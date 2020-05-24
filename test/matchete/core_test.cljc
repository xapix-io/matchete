(ns matchete.core-test
  (:require [matchete.core :as sut #?@(:cljs (:include-macros true))]
            [matchete.matcher :as m]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true]))
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(deftest core-test
  (is (= '({x :x
            y :y
            obj {:x :x
                  :y :y}
            k 1
            v 1}
           {x :x
            y :y
            obj {:x :x
                  :y :y}
            k 4
            v 4})
         (sut/matches
          '[1 "qwe" x
            {:x x
             :collections [1 2 3 x]}
            [1 2 3 & _]
            [1 2 3 4]
            (and obj {:x x
                            :y y})
            (or 1 x)
            {k v}
            _]
          [1 "qwe" :x
           {:x :x
            :collections [1 2 3 :x]}
           [1 2 3 4]
           [1 2 3 4]
           {:x :x
            :y :y}
           :x
           {1 1
            4 4}
           :not-bind]))))

(deftest failed-binding
  (is (not (sut/match? '{:x x
                         :y x}
                       {:x 1
                        :y 2})))
  (is (not (sut/match? '[1 2 3 & _]
                       {:x 1}))))

(deftest failed-and
  (is (not (sut/match? '{:x x
                         :y (and y x)}
                       {:x 1
                        :y 2}))))

(deftest failed-seq
  (is (not (sut/match? '[1 2 3]
                       {:x 1}))))

(deftest failed-map
  (is (not (sut/match? '{:x 1
                         :y 2}
                       {:x 1}))))

(deftest pattern-as-a-key
  (is (= [{'key :foo}]
         (sut/matches '{(and key :foo) 1}
                      {:foo 1}))))

(deftest precompiled-matcher
  (let [M (m/matcher '{x y
                       z v})]
    (is (= '({x :x, y 1, z :y, v 2}
             {x :x, y 1, z :z, v 3}
             {x :y, y 2, z :x, v 1}
             {x :y, y 2, z :z, v 3}
             {x :z, y 3, z :x, v 1}
             {x :z, y 3, z :y, v 2})
           (sut/matches M {:x 1 :y 2 :z 3})))))

(sut/defn-match foo
   ([?x]                (+ ?x 1))
   ([?x {:foo 1 ?n ?n}] [?x ?n]))

(deftest macro-test
  (is (= [2]
         (foo 1)))
  (is (thrown-with-msg? ExceptionInfo
                        #"Can not find declaration that satisfy the arguments"
                        (foo 1 2)))
  (is (try
        (foo 1 2)
        (catch ExceptionInfo e
          (is (= {:arguments [1 2], :patterns '([?x] [?x {:foo 1, ?n ?n}])}
                 (ex-data e))))))
  (is (= [[1 :bar]]
         (foo 1 {:foo 1 :bar :bar}))))
