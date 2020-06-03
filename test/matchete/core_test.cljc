(ns matchete.core-test
  (:require [matchete.core :as sut]
            [example.poker-hand :as ph]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is are]]
               :cljs [cljs.test :refer [deftest is are] :include-macros true]))
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(deftest core-test
  (is (= ['{?x :x
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
            ?v 4}]
         (sut/matches
          '[1 "qwe" ?x
            {:x ?x
             :collections [1 2 3 ?x]}
            [1 2 3 & _]
            [1 2 3 4]
            (cat ?obj {:x ?x
                       :y ?y})
            (alt 1 ?x)
            {?k ?v}
            #{1 2 3}
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
           #{1 2 3}
           :not-bind]))))

(deftest memo-binding
  (is (= ['{!foo [1 3]
            !bar [2 4]}]
         (sut/matches '[!foo !bar !foo !bar]
                      [1 2 3 4])))
  (is (= '({!path [:x :x], ?node 1}
           {!path [:x :y], ?node []}
           {!path [:x :x], ?node 1})
         (sut/matches '{:foo (scan {!path {!path ?node}})}
                      {:foo [{:x {:x 1 :y []}} {:x {:x 1}}]}))))

(deftest pattern?
  (is (not (sut/pattern? {:foo 1 :bar 2})))
  (is (sut/pattern? (with-meta (fn []) {::sut/matcher? true}))))

(deftest scan-pattern
  (is (empty? (sut/matches '(scan {:foo ?x})
                           [{:bar 1} {:bar 2}])))
  (is (empty? (sut/matches '(scan {:foo ?x})
                           {:foo 1})))
  (is (= ['{?x 1}]
         (sut/matches '(scan {:foo ?x})
                      [{:foo 1}])))
  (is (= ['{?x 1}
          '{?x 2}]
         (sut/matches '(scan {:foo ?x})
                      [{:foo 1}
                       {}
                       {:foo 2}])))
  (is (= #{'{?x 1} '{?x 2}}
         (set (sut/matches '(scan {:foo ?x})
                           #{{:foo 1}
                             {}
                             {:foo 2}}))))
  (is (= #{'{?x 1 ?y 3 ?z 2}
           '{?x 1 ?y 3 ?z 4}}
         (set ((sut/matcher '#{?x ?y ?z})
               '{?x 1
                 ?y 3}
               #{1 2 3 4})))))

(deftest scan-indexed-pattern
  (is (empty? (sut/matches '(scan-indexed ?index ?data)
                           [])))
  (is (empty? (sut/matches '(scan-indexed ?index ?data)
                           {})))
  (is (empty? (sut/matches '(scan-indexed ?index ?data)
                           42)))
  (is (= #{'{?index 1 ?data 2}
           '{?index 0 ?data 1}
           '{?index 2 ?data 3}}
         (set (sut/matches '(scan-indexed ?index ?data)
                           [1 2 3]))
         (set (sut/matches '(scan-indexed ?index ?data)
                           {0 1
                            1 2
                            2 3})))))

(deftest each-test
  (let [rules {'$even? (fn [matches _ n]
                         (when ((every-pred number? even?) n)
                           (list matches)))
               '$odd? (fn [matches _ n]
                        (when ((every-pred number? odd?) n)
                          (list matches)))}]
    (is (= ['{!odd [1 3], !even [2]}]
           (sut/matches '(each (alt (cat $even? !even)
                                    (cat $odd? !odd)))
                        rules
                        [1 2 3])))))

(deftest each-indexed-test
  (let [rules {'$max (fn [{:syms [?max-element ?current-index] :as matches} _ n]
                       (list (if (and ?max-element
                                      (> ?max-element n))
                               (select-keys matches ['?max-element '?max-index])
                               {'?max-element n
                                '?max-index ?current-index})))}
        sample (shuffle (range 100))]
    (is (= [{'?max-element 99 '?max-index (ffirst (filter (fn [[_ v]] (= v 99))
                                                          (map-indexed vector sample)))}]
           (sut/matches '(each-indexed ?current-index $max) rules sample)))))

(deftest rule-tests
  (is (= #{'{!path [:array 1 :x]}
           '{!path [:foo :bar :baz]}}
         (set (sut/matches '(scan-indexed !path
                                          (scan-indexed !path
                                                        (scan-indexed !path 42)))
                           {:foo {:bar {:baz 42
                                        :zab 24}}
                            :array [{:x 1}
                                    {:x 42}]}))
         (set (sut/matches '(def-rule $path-to-42
                              (scan-indexed !path (alt $path-to-42 42)))
                           {:foo {:bar {:baz 42
                                        :zab 24}}
                            :array [{:x 1}
                                    {:x 42}]}))
         (set ((sut/matcher '$path-to-42)
               {} {'$path-to-42 (sut/matcher '(scan-indexed !path (alt $path-to-42 42)))}
               {:foo {:bar {:baz 42
                            :zab 24}}
                :array [{:x 1}
                        {:x 42}]}))

         (set (sut/matches '$path-to-42
               '{$path-to-42 (scan-indexed !path (alt $path-to-42 42))}
               {:foo {:bar {:baz 42
                            :zab 24}}
                :array [{:x 1}
                        {:x 42}]}))))

  (is (= #{'{!path [:array 1 :x], ?node 42}
           '{!path [:foo :bar :zab], ?node 24}
           '{!path [:array 1 :y 3], ?node 4}
           '{!path [:array 1 :y 1], ?node 2}
           '{!path [:foo :bar :baz], ?node 42}}
         (set ((sut/matcher '$path-to-even)
               {} {'$path-to-even (sut/matcher '(scan-indexed !path (alt $path-to-even (cat $even? ?node))))
                   '$even? ^::sut/matcher? (fn [matches _rules data]
                                             (when (and (number? data) (even? data))
                                               (list matches)))}
               {:foo {:bar {:baz 42
                            :zab 24}}
                :array [{:x 1}
                        {:x 42
                         :y [1 2 3 4]}]}))))

  (is (thrown-with-msg? ExceptionInfo
                        #"Undefined rule"
                        (sut/matches '(scan $rule)
                                     [1 2 3])))

  (is (thrown-with-msg? ExceptionInfo
                        #"Undefined rule"
                        (sut/matches '(scan (%plus 1 ?n))
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
  (is (= ['{?key :foo}]
         (sut/matches '{?key 1}
                      {:foo 1}))))

(deftest precompiled-matcher
  (let [M (sut/matcher '{?x ?y
                         ?z ?v})]
    (is (= ['{?x :x, ?y 1, ?z :y, ?v 2}
            '{?x :x, ?y 1, ?z :z, ?v 3}
            '{?x :y, ?y 2, ?z :x, ?v 1}
            '{?x :y, ?y 2, ?z :z, ?v 3}
            '{?x :z, ?y 3, ?z :x, ?v 1}
            '{?x :z, ?y 3, ?z :y, ?v 2}]
           (sut/matches M {:x 1 :y 2 :z 3})))))

(deftest functional-form
  (let [find-leafs (sut/matcher
                    (sut/def-rule '$find-leafs
                      (sut/alt (sut/scan-indexed '!path '$find-leafs) '?node)))]
    (are [x y] (= x (find-leafs y))
      '({?node 1}) 1

      '({?node nil}) nil

      '({!path [:x], ?node 1}
        {!path [:y 0], ?node 2}
        {!path [:y 1], ?node 3}
        {!path [:y 2], ?node 4})
      {:x 1 :y [2 3 4]})))

(deftest poker-hand
  (are [hand res] (= (ph/poker-hand hand) res)
    #{[:♣ 10] [:♣ 14] [:♣ 12] [:♣ 13] [:♣ 11]} "Royal flush"

    #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 8] [:♠ 9]} "Straight flush"

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 5]} "Four of a kind"

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 7]} "Full house"

    #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 13] [:♠ 9]} "Flush"

    #{[:♠ 5] [:♣ 6] [:♠ 7] [:♠ 8] [:♠ 9]} "Straight"

    #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 8]} "Three of a kind"

    #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 10]} "Two pair"

    #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 8]} "One pair"

    #{[:♠ 8] [:♠ 5] [:♠ 6] [:♦ 11] [:♠ 7]} [:♦ 11]))

(deftest not-pattern
  (letfn [(matches [data]
            (sut/matches '{:foo ?foo
                           :bar (cat ?bar (not (%starts-with "__")))}
                         {'%starts-with (fn [pref]
                                          (fn [matches _ data]
                                            (when (string/starts-with? data pref)
                                              (list matches))))}
                         data))]
    (are [data res] (= res (matches data))
      {:foo 1 :bar "qwe"}   ['{?bar "qwe" ?foo 1}]

      {:foo 1 :bar "__qwe"} [])))

(deftest incorrect-tail-pattern
  (is (thrown-with-msg? ExceptionInfo
                        #"Destructuring of a sequence tail must be a single pattern"
                        (sut/matcher '[?x & ?y ?z])))
  (try
    (sut/matcher '[?x & ?y ?z])
    (catch ExceptionInfo e
      (is (= {:pattern '(?y ?z)}
             (ex-data e))))))
