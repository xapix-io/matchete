(ns matchete.core-test
  (:require [matchete.core :as sut]
            [matchete.matcher :as m]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])))

(deftest core-test
  (is (= '({?x :x
            ?y :y
            ?obj {:x :x
                  :y :y}
            ?k 1
            ?v 1}
           {?x :x
            ?y :y
            ?obj {:x :x
                  :y :y}
            ?k 4
            ?v 4})
         (sut/matches
          '[1 "qwe" ?x
            {:x ?x
             collections [1 2 3 ?x]}
            [1 2 3]
            (exact [1 2 3 4])
            (cat ?obj {:x ?x
                       :y ?y})
            (alt 1 ?x)
            {?k ?v}
            _]
          [1 "qwe" :x
           {:x :x
            'collections [1 2 3 :x]}
           [1 2 3 4]
           [1 2 3 4]
           {:x :x
            :y :y}
           :x
           {1 1
            4 4}
           :not-bind]))))

(deftest failed-binding
  (is (not (sut/match? '{:x ?x
                         :y ?x}
                       {:x 1
                        :y 2}))))

(deftest failed-cat
  (is (not (sut/match? '{:x ?x
                         :y (cat ?y ?x)}
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
  (is (= [{'?key :foo}]
         (sut/matches '{(cat ?key :foo) 1}
                      {:foo 1}))))

(deftest memo-binding
  (is (= [{'!vals [1 2 3 4 5]}]
         (sut/matches '{:x !vals
                        :y !vals
                        :z [!vals !vals !vals]}
                      {:x 1
                       :y 2
                       :z [3 4 5 6]}))))

(deftest precompiled-matcher
  (let [M (m/matcher '{?x ?y
                       ?z ?v})]
    (is (= '({?x :x, ?y 1, ?z :y, ?v 2}
             {?x :x, ?y 1, ?z :z, ?v 3}
             {?x :y, ?y 2, ?z :x, ?v 1}
             {?x :y, ?y 2, ?z :z, ?v 3}
             {?x :z, ?y 3, ?z :x, ?v 1}
             {?x :z, ?y 3, ?z :y, ?v 2})
           (sut/matches M {:x 1 :y 2 :z 3})))))
