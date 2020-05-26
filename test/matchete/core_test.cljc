(ns matchete.core-test
  (:require [matchete.core :as sut #?@(:cljs (:include-macros true))]
            [matchete.matcher :as m]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true]))
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(deftest core-test
  (is (= #{'{?x :x
             ?y :y
             ?obj {:x :x
                   :y :y}
             ?k 1
             ?v 1}
           '{?x :x
             ?y :y
             ?obj {:x :x
                   :y :y}
             ?k 4
             ?v 4}}
         (sut/matches
          '[1 "qwe" ?x
            {:x ?x
             :collections [1 2 3 ?x]}
            [1 2 3 & _]
            [1 2 3 4]
            (and ?obj {:x ?x
                       :y ?y})
            (or 1 ?x)
            {?k ?v}
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

(deftest memo-binding
  (is (= #{'{!foo [1 3]
             !bar [2 4]}}
         (sut/matches '[!foo !bar !foo !bar]
                      [1 2 3 4]))))

(deftest scan-pattern
  (is (= #{}
         (sut/matches '(scan {:foo ?x})
                      [{:bar 1} {:bar 2}])
         (sut/matches '(scan {:foo ?x})
                      {:foo 1})))
  (is (= #{'{?x 1}}
         (sut/matches '(scan {:foo ?x})
                      [{:foo 1}])))
  (is (= #{'{?x 1}
           '{?x 2}}
         (sut/matches '(scan {:foo ?x})
                      [{:foo 1}
                       {}
                       {:foo 2}]))))

(deftest scan-indexed-pattern
  (is (= #{}
         (sut/matches '(scan-indexed ?index ?data)
                      [])
         (sut/matches '(scan-indexed ?index ?data)
                      {})
         (sut/matches '(scan-indexed ?index ?data)
                      42)))
  (is (= #{'{?index 1 ?data 2}
           '{?index 0 ?data 1}
           '{?index 2 ?data 3}}
         (sut/matches '(scan-indexed ?index ?data)
                      [1 2 3])
         (sut/matches '(scan-indexed ?index ?data)
                      {0 1
                       1 2
                       2 3}))))

(deftest rule-tests
  (is (= #{'{!path [:array 1 :x]}
           '{!path [:foo :bar :baz]}}
         (sut/matches '(scan-indexed !path
                                     (scan-indexed !path
                                                   (scan-indexed !path 42)))
                      {:foo {:bar {:baz 42
                                   :zab 24}}
                       :array [{:x 1}
                               {:x 42}]})
         (sut/matches '(rule $path-to-42
                             (scan-indexed !path (or $path-to-42 42)))
                      {:foo {:bar {:baz 42
                                   :zab 24}}
                       :array [{:x 1}
                               {:x 42}]})
         ((sut/matcher '$path-to-42)
          {} {'$path-to-42 (sut/matcher '(scan-indexed !path (or $path-to-42 42)))}
          {:foo {:bar {:baz 42
                       :zab 24}}
           :array [{:x 1}
                   {:x 42}]})))

  (is (= #{'{!path [:array 1 :x], ?node 42}
           '{!path [:foo :bar :zab], ?node 24}
           '{!path [:array 1 :y 3], ?node 4}
           '{!path [:array 1 :y 1], ?node 2}
           '{!path [:foo :bar :baz], ?node 42}}
         ((sut/matcher '$path-to-even)
          {} {'$path-to-even (sut/matcher '(scan-indexed !path (or $path-to-even (and $even? ?node))))
              '$even? (fn [matches _scope data]
                        (when (and (number? data) (even? data))
                          (list matches)))}
          {:foo {:bar {:baz 42
                       :zab 24}}
           :array [{:x 1}
                   {:x 42
                    :y [1 2 3 4]}]})))

  (is (thrown-with-msg? ExceptionInfo
                        #"Undefined rule"
                        (sut/matches '(scan $rule)
                                     [1 2 3])))

  (try
    (sut/matches '(scan $rule)
                 [1 2 3])
    (catch ExceptionInfo e
      (is (= {:rule '$rule}
             (ex-data e))))))

(deftest failed-binding
  (is (not (sut/match? '{:x ?x
                         :y ?x}
                       {:x 1
                        :y 2})))
  (is (not (sut/match? '[1 2 3 & _]
                       {:x 1}))))

(deftest failed-and
  (is (not (sut/match? '{:x ?x
                         :y (and ?y ?x)}
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
  (is (= #{'{?key :foo}}
         (sut/matches '{?key 1}
                      {:foo 1}))))

(deftest precompiled-matcher
  (let [M (m/matcher '{?x ?y
                       ?z ?v})]
    (is (= #{'{?x :x, ?y 1, ?z :y, ?v 2}
             '{?x :x, ?y 1, ?z :z, ?v 3}
             '{?x :y, ?y 2, ?z :x, ?v 1}
             '{?x :y, ?y 2, ?z :z, ?v 3}
             '{?x :z, ?y 3, ?z :x, ?v 1}
             '{?x :z, ?y 3, ?z :y, ?v 2}}
           (sut/matches M {:x 1 :y 2 :z 3})))))

(deftest incorrect-tail-pattern
  (is (thrown-with-msg? ExceptionInfo
                        #"Destructuring of a sequence tail must be a single pattern"
                        (sut/matcher '[?x & ?y ?z])))
  (try
    (sut/matcher '[?x & ?y ?z])
    (catch ExceptionInfo e
      (is (= {:pattern '(?y ?z)}
             (ex-data e))))))

(deftest incorrect-or-pattern
  (is (thrown-with-msg? ExceptionInfo
                        #"`or` expect more than one pattern"
                        (sut/matcher '(or ?x))))
  (try
    (sut/matcher '(or ?x))
    (catch ExceptionInfo e
      (is (= {:pattern '(or ?x)}
             (ex-data e))))))

(deftest incorrect-and-pattern
  (is (thrown-with-msg? ExceptionInfo
                        #"`and` expect more than one pattern"
                        (sut/matcher '(and ?x))))
  (try
    (sut/matcher '(and ?x))
    (catch ExceptionInfo e
      (is (= {:pattern '(and ?x)}
             (ex-data e))))))

(deftest incorrect-scan-pattern
  (is (thrown-with-msg? ExceptionInfo
                        #"`scan` expect exactly one pattern"
                        (sut/matcher '(scan ?x ?y))))
  (try
    (sut/matcher '(scan ?x ?y))
    (catch ExceptionInfo e
      (is (= {:pattern '(scan ?x ?y)}
             (ex-data e))))))

(deftest incorrect-scan-indexed-pattern
  (is (thrown-with-msg? ExceptionInfo
                        #"`scan-indexed` expect exactly two patterns"
                        (sut/matcher '(scan-indexed ?x ?y ?z))))
  (try
    (sut/matcher '(scan-indexed ?x ?y ?z))
    (catch ExceptionInfo e
      (is (= {:pattern '(scan-indexed ?x ?y ?z)}
             (ex-data e))))))
