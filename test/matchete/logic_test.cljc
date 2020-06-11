(ns matchete.logic-test
  (:require [matchete.logic :as logic]
            [example.poker-hand :as ph]
            [example.graph :as g]
            #?(:clj [clojure.test :refer [deftest is are]]
               :cljs [cljs.test :refer [deftest is are] :include-macros true])))

(deftest core-test
  (is (= [{:?x :x
           :?y :y
           :?obj {:x :x
                  :y :y}
           :?k 1
           :?v 1}
          {:?x :x
           :?y :y
           :?obj {:x :x
                  :y :y}
           :?k 4
           :?v 4}]
         (logic/matches (logic/matcher [1 "qwe" :?x
                                        {:x :?x
                                         :collections [1 2 3 :?x]}
                                        [1 2 3]
                                        [1 2 3 4]
                                        (logic/conj :?obj {:x :?x
                                                           :y :?y})
                                        (logic/disj 1 :?x)
                                        {:?k :?v}
                                        #{1 2 3}
                                        :_])
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

(deftest set-pattern
  (is (= []
         (logic/matches (logic/matcher #{:?x :?y 42})
                        #{1 2 3})))
  (is (= [{:?x 1, :?y 3}
          {:?x 1, :?y 2}
          {:?x 3, :?y 1}
          {:?x 3, :?y 2}
          {:?x 2, :?y 1}
          {:?x 2, :?y 3}]
         (logic/matches (logic/matcher #{:?x :?y 42})
                        #{1 2 3 42}))))

(deftest not-pattern
  (is (= []
         (logic/matches (logic/matcher (logic/each (logic/not 42)))
                        [1 2 3 42])))
  (is (= [{}]
         (logic/matches (logic/matcher (logic/each (logic/not 42)))
                        [1 2 3 4]))))

(deftest no-match
  (is (every? empty?
              [(logic/matches (logic/matcher [:?x :?x]) [1 2])
               (logic/matches (logic/matcher (logic/conj :?x 42)) 43)
               (logic/matches (logic/matcher [1 2 3]) "qwe")])))

(deftest placeholders
  (is (= [{:_user-name "Bob", :?recipient 2}
          {:_user-name "Bob", :?recipient 3}]
         (logic/matches (logic/matcher {:id :_
                                        :name :_user-name
                                        :messages (logic/scan {:author :_user-name
                                                               :dest :?recipient})})
                        {:id 1
                         :name "Bob"
                         :messages [{:author "Bob"
                                     :dest 2}
                                    {:author "Alise"
                                     :dest 1}
                                    {:author "Bob"
                                     :dest 3}]}))))

(deftest memo-binding
  (is (= [{:!foo [1 3]
           :!bar [2 4]}]
         (logic/matches (logic/matcher [:!foo :!bar :!foo :!bar])
                        [1 2 3 4])))
  (is (= [{:!path [:x :x]
           :?node 1}
          {:!path [:x :y]
           :?node []}
          {:!path [:x :x]
           :?node 1}]
         (logic/matches (logic/matcher {:foo (logic/scan {:!path {:!path :?node}})})
                        {:foo [{:x {:x 1 :y []}} {:x {:x 1}}]}))))

(deftest pattern?
  (is (not (logic/pattern? {:foo 1 :bar 2})))
  (is (logic/pattern? (reify logic/Pattern
                        (matches [_ _ _]))))
  (is (logic/pattern? {:?foo 1}))
  (is (logic/pattern? {(logic/conj 42 :?x) "qwe"})))

(deftest scan-pattern
  (is (empty? (logic/matches (logic/matcher (logic/scan {:foo :?x}))
                             [{:bar 1} {:bar 2}])))
  (is (empty? (logic/matches (logic/matcher (logic/scan {:foo :?x}))
                             {:foo 1})))
  (is (= [{:?x 1}]
         (logic/matches (logic/matcher (logic/scan {:foo :?x}))
                        [{:foo 1}])))
  (is (= [{:?x 1}
          {:?x 2}]
         (logic/matches (logic/matcher (logic/scan {:foo :?x}))
                        [{:foo 1}
                         {}
                         {:foo 2}]))))

(deftest scan-indexed-pattern
  (is (empty? (logic/matches (logic/matcher (logic/scan :?index :?data))
                             [])))
  (is (empty? (logic/matches (logic/matcher (logic/scan :?index :?data))
                             {})))
  (is (empty? (logic/matches (logic/matcher (logic/scan :?index :?data))
                             42)))
  (is (= [{:?index 0
           :?data 1}
          {:?index 1
           :?data 2}
          {:?index 2
           :?data 3}]
         (logic/matches (logic/matcher (logic/scan :?index :?data))
                        [1 2 3]))))

(deftest each-test
  (is (= [#:user{:!ids [1 2 3]}]
         (logic/matches (logic/matcher {:users (logic/each {:id :user/!ids})})
                        {:users [{:id 1
                                  :name "Alise"}
                                 {:id 2}
                                 {:id 3
                                  :name "Bob"}]}))))

(deftest each-indexed-test
  (is (= [#:user{:!ids [0 1 2]}]
         (logic/matches (logic/matcher {:users (logic/each :user/!ids {:id :_})})
                        {:users [{:id 1
                                  :name "Alise"}
                                 {:id 2}
                                 {:id 3
                                  :name "Bob"}]}))))

(deftest failed-binding
  (is (not (logic/match? (logic/matcher {:x :?x :y :?x}) {:x 1 :y 2})))
  (is (not (logic/match? (logic/matcher [1 2 3]) {:x 1})))
  (is (not (logic/match? (logic/matcher {:x :?x}) {:?x 1} {:x 2}))))

(deftest failed-conjn
  (is (not (logic/match? (logic/matcher {:x :?x
                                         :y (logic/conj :?y :?x)})
                         {:x 1
                          :y 2}))))

(deftest failed-seq
  (is (not (logic/match? (logic/matcher [1 2 3])
                         {:x 1}))))

(deftest failed-map
  (is (not (logic/match? (logic/matcher {:x 1
                                         :y 2})
                         {:x 1}))))

(deftest aggregate-rule
  (letfn [(minimum [s]
            (reify logic/Pattern
              (matches [_ preconditions n]
                (let [prev-min (get preconditions s n)]
                  (list (assoc preconditions s (min prev-min n)))))))]
    (is (= [{:?min-n 1}]
           (logic/matches (logic/matcher (repeat 3 (minimum :?min-n)))
                          [2 3 1 0])))))

(declare children)

(defn children []
  (reify logic/Pattern
    (matches [_ preconditions data]
      (logic/matches (logic/matcher
                      (logic/disj (logic/scan :!path (children))
                                  :?node))
                     preconditions data))))

(deftest recursive-matcher
  (is (= [{:!path [0], :?node 1}
          {:!path [1 :foo 3], :?node 3}
          {:!path [1 :foo 2], :?node 2}
          {:!path [2], :?node 3}]
         (logic/matches (logic/matcher (children)) [1 {:foo #{2 3}} 3]))))

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
