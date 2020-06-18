(ns matchete.core-test
  (:require [matchete.core :as mc]
            [example.poker-hand :as ph]
            [example.graph :as g]
            #?(:clj [clojure.test :refer [deftest are is]]
               :cljs [cljs.test :refer [deftest are is] :include-macros true])))

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

(deftest graph
  (is (= 46 (g/shortest-path g/city-to-city-distance "Berlin"))))
