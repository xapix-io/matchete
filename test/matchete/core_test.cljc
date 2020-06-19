(ns matchete.core-test
  (:require [matchete.core :as mc :include-macros true]
            [example.poker-hand :as ph]
            [example.graph :as g]
            #?(:clj [clojure.test :refer [deftest are is]]
               :cljs [cljs.test :refer [deftest are is] :include-macros true])))

(deftest poker-hand
  (are [hand res] (= res (ph/poker-hand hand))
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

(deftest graph
  (is (= 46 (g/shortest-path g/city-to-city-distance "Berlin"))))

(deftest map-pattern
  (is (= [{}]
         ((mc/matcher {:a "a"
                       :b "b"})
          {:a "a"
           :b "b"
           :c "c"})))
  (is (not (mc/match? {:a '?a
                       :b '?a}
                      {:a "a"
                       :b "b"})))
  (is (not (mc/match? {:a "a"} "string")))
  (is (not (mc/match? {:a "a" :b "b"} {:a "a" :c "c"})))
  (is (not (mc/match? {:a "a" :b "b"} {:a "a"})))
  (is (= '[{?b "this-is-b", ?c :c}
           {?b "this-is-b", ?c :d}
           {?b "this-is-b", ?c :e}]
         ((mc/matcher {:a 42
                       :b '?b
                       '?c 42
                       (mc/formula (keyword ?b)) 24
                       {:x (mc/predicate string?)} true})
          {:a 42
           :b "this-is-b"
           :c 42
           :d 42
           :this-is-b 24
           :e 42
           {:x "qwe"} true
           {:x 42} true}))))

(deftest set-pattern
  (is (= [{}]
         ((mc/matcher #{1 2 3}) #{1 2 3 4 5})))
  (is (not (mc/match? #{1 2 3} #{1 2})))
  (is (not (mc/match? #{1 2 3} "string")))
  (is (not (mc/match? #{1 2 3} #{1 3 4})))
  (is (= '#{{?x 1}
            {?x 2}
            {?x 3}}
         (set ((mc/matcher #{'?x}) #{1 2 3})))))

(deftest placeholder-pattern
  (is (= [{}]
         (mc/matches '[?_ ?_ ?_a ?_a] ["ignore" "another-ignore" 1 1])))
  (is (= '[{?_a 1}]
         ((mc/matcher '[?_ ?_ ?_a ?_a]) ["ignore" "another-ignore" 1 1]))))

(deftest formula-pattern
  (is (= []
         ((mc/matcher [(mc/formula (+ ?x 20)) '?x]) [42 23])))
  (is (= [{'?x 22 '?y 42}]
         ((mc/matcher [(mc/formula (+ ?x 20) ?y) '?x]) [42 22])))
  (is (= [{'?x 22 '?y 1}]
         ((mc/matcher [(mc/formula (+ ?x 20)) (mc/formula (* ?y ?x 10)) '?x '?y]) [42 220 22 1]))))

(deftest result-of-pattern
  (let [inc-x (with-meta
                (fn [{:syms [?x]}]
                  (inc ?x))
                {:lvars ['?x]})
        sum-x-y (with-meta
                  (fn [{:syms [?x ?y]}]
                    (+ ?x ?y))
                  {:lvars ['?x '?y]})]
    (is (= [{'?x 1 '?y 2}]
           (mc/matches ['?x (mc/result-of inc-x '?y)] [1 2])))
    (is (= [{'?x 1 '?y 2}]
           (mc/matches ['?x (mc/result-of sum-x-y) '?y]
                       [1 3 2])))
    (is (not (mc/match? [(mc/result-of sum-x-y) '?y]
                        [3 2])))))

(deftest lvar-pattern
  (is (not (mc/match? ['?x '?x] [1 2]))))

(defn conj-path [path step]
  ((fnil conj []) path step))

(mc/defpattern tree-walk
  (mc/or (mc/scan (mc/aggregate-by conj-path '?path) tree-walk)
         '?leaf))

(mc/defnpattern limited-tree-walk [{:syms [?path]}]
  (if (< (count ?path) 3)
    (mc/or (mc/scan [(mc/aggregate-by conj-path '?path) limited-tree-walk])
           '?leaf)
    '?leaf))

(deftest recursive-pattern
  (is (= '[{?path [:x 0], ?leaf 1}
           {?path [:x 1], ?leaf 2}
           {?path [:x 2 :y], ?leaf "qwe"}
           {?path [:z], ?leaf 42}]
         (mc/matches tree-walk {:x [1 2 {:y "qwe"}]
                                :z 42})))
  (is (= '[{?path [:x :x :x], ?leaf {:x 1}}
           {?path [:y], ?leaf 42}]
         (mc/matches limited-tree-walk {:x {:x {:x {:x 1}}}
                                        :y 42}))))

(deftest each-pattern
  (is (not (mc/match? (mc/each 42) [42 3 42])))
  (is (mc/match? (mc/each 42) [42 42 42])))

(deftest some-pattern
  (is (mc/match? (mc/some 42) [42 3 42])))

(deftest predicate-pattern
  (is (mc/match? (mc/matcher (mc/each (mc/predicate string?))) ["qwe" "rty" "uio"]))
  (is (not (mc/match? (mc/each (mc/predicate string?)) ["qwe" 'rty "uio"])))
  (is (= [{'?string "qwe"}]
         (mc/matches (mc/matcher {:string (mc/predicate string? '?string)
                                  :number (mc/predicate number?)})
                     {:string "qwe"
                      :number 42
                      :boolean true}))))

(deftest and-pattern
  (is (not (mc/match? ['?x (mc/and 42 '?x)] [43 42])))
  (is (= [{'?x true}] (mc/matches (mc/and true '?x) true))))

(deftest or-pattern
  (is (mc/match? (mc/or 42) 42))
  (is (not (mc/match? (mc/or (mc/predicate string?) (mc/predicate number?)) nil))))

(deftest not-pattern
  (is (mc/match? (mc/not (mc/predicate string?)) 42))
  (is (not (mc/match? (mc/not (mc/predicate string?)) "42"))))

(comment

  (mc/matches (mc/and (mc/scan (mc/formula (Math/sqrt (+ (Math/pow ?x 2) (Math/pow ?y 2))) ?z))
                      (mc/scan '?x)
                      (mc/scan '?y))
              (map double (range 1 50)))

  )
