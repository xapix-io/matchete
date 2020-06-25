(ns matchete.data-form-test
  (:require [matchete.data-form :as df]
            [matchete.core :as mc]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])))

(deftest simple
  (is (= [{'?x 1}]
         (mc/matches (df/make-pattern '[:predicate [:fn (fn [x] true)] ?x]) 1))))

(deftest function
  (is (= 3 ((df/make-pattern '[:fn (fn [x] (inc x))]) 2))))

(deftest tree-walk
  (let [M (df/make-pattern '[:or {:id tree-walk}
                             [:scan !path [:ref tree-walk]]
                             ?leaf])
        M' (df/make-pattern '[:or {:id tree-walk}
                              [:and
                               [:guard [:fn (fn [{:syms [?path] :as m}]
                                              (< (count ?path) 3))]]
                               [:scan !path [:ref tree-walk]]]
                              ?leaf])]
    (is (= '[{?path [:x 0], ?leaf 1}
             {?path [:x 1], ?leaf 2}
             {?path [:x 2 :y], ?leaf "qwe"}
             {?path [:z], ?leaf 42}]
           (mc/matches M {:x [1 2 {:y "qwe"}]
                          :z 42})))
    (is (= '[{?path [:x :x :x], ?leaf {:x 1}}
             {?path [:y], ?leaf 42}]
           (mc/matches M' {:x {:x {:x {:x 1}}}
                           :y 42})))))
