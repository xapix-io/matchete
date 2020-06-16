(ns matchete.lang-test
  (:require [matchete.lang :as ml]
            [matchete.lang.core :as mlcore]
            [matchete.lang.string :as mlstring]
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
         (ml/matches (ml/matcher [1 "qwe" :?x
                                  {:x :?x
                                   :collections [1 2 3 :?x]}
                                  [1 2 3]
                                  [1 2 3 4]
                                  (mlcore/and :?obj {:x :?x
                                                     :y :?y})
                                  (mlcore/or 1 :?x)
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
         (ml/matches (ml/matcher #{:?x :?y 42})
                     #{1 2 3})))
  (is (= [{:?x 1, :?y 3}
          {:?x 1, :?y 2}
          {:?x 3, :?y 1}
          {:?x 3, :?y 2}
          {:?x 2, :?y 1}
          {:?x 2, :?y 3}]
         (ml/matches (ml/matcher #{:?x :?y 42})
                     #{1 2 3 42}))))

(deftest no-match
  (is (every? empty?
              [(ml/matches (ml/matcher [:?x :?x]) [1 2])
               (ml/matches (ml/matcher (mlcore/and :?x 42)) 43)
               (ml/matches (ml/matcher [1 2 3]) "qwe")])))

(deftest placeholders
  (is (= [{:_user-name "Bob", :?recipient 2}
          {:_user-name "Bob", :?recipient 3}]
         (ml/matches (ml/matcher {:id :_
                                  :name :_user-name
                                  :messages (mlcore/scan {:author :_user-name
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
         (ml/matches (ml/matcher [:!foo :!bar :!foo :!bar])
                     [1 2 3 4])))
  (is (= [{:!path [:x :x]
           :?node 1}
          {:!path [:x :y]
           :?node []}
          {:!path [:x :x]
           :?node 1}]
         (ml/matches (ml/matcher {:foo (mlcore/scan {:!path {:!path :?node}})})
                     {:foo [{:x {:x 1 :y []}} {:x {:x 1}}]}))))

(deftest pattern?
  (is (not (ml/pattern? {:foo 1 :bar 2})))
  (is (ml/pattern? (reify ml/Pattern
                     (matches [_ _ _]))))
  (is (ml/pattern? {:?foo 1}))
  (is (ml/pattern? {(mlcore/and 42 :?x) "qwe"})))

(deftest scan-pattern
  (is (empty? (ml/matches (ml/matcher (mlcore/scan {:foo :?x}))
                          [{:bar 1} {:bar 2}])))
  (is (empty? (ml/matches (ml/matcher (mlcore/scan {:foo :?x}))
                          {:foo 1})))
  (is (= [{:?x 1}]
         (ml/matches (ml/matcher (mlcore/scan {:foo :?x}))
                     [{:foo 1}])))
  (is (= [{:?x 1}
          {:?x 2}]
         (ml/matches (ml/matcher (mlcore/scan {:foo :?x}))
                     [{:foo 1}
                      {}
                      {:foo 2}]))))

(deftest scan-indexed-pattern
  (is (empty? (ml/matches (ml/matcher (mlcore/scan :?index :?data))
                          [])))
  (is (empty? (ml/matches (ml/matcher (mlcore/scan :?index :?data))
                          {})))
  (is (empty? (ml/matches (ml/matcher (mlcore/scan :?index :?data))
                          42)))
  (is (= [{:?index 0
           :?data 1}
          {:?index 1
           :?data 2}
          {:?index 2
           :?data 3}]
         (ml/matches (ml/matcher (mlcore/scan :?index :?data))
                     [1 2 3]))))

(deftest each-test
  (is (= [#:user{:!ids [1 2 3]}]
         (ml/matches (ml/matcher {:users (mlcore/every? {:id :user/!ids})})
                     {:users [{:id 1
                               :name "Alise"}
                              {:id 2}
                              {:id 3
                               :name "Bob"}]}))))

(deftest each-indexed-test
  (is (= [#:user{:!ids [0 1 2]}]
         (ml/matches (ml/matcher {:users (mlcore/every? :user/!ids {:id :_})})
                     {:users [{:id 1
                               :name "Alise"}
                              {:id 2}
                              {:id 3
                               :name "Bob"}]}))))

(deftest some-test
  (is (= [#:user{:!ids [1 3], :!names ["Alise" "Bob"]}]
         (ml/matches (ml/matcher {:users (mlcore/some {:id :user/!ids
                                                       :name :user/!names})})
                     {:users [{:id 1
                               :name "Alise"}
                              {:id 2}
                              {:id 3
                               :name "Bob"}]}))))

(deftest some-indexed-test
  (is (= [#:user{:!ids [0 2]}]
         (ml/matches (ml/matcher {:users (mlcore/some :user/!ids {:id :_ :name :_})})
                     {:users [{:id 1
                               :name "Alise"}
                              {:id 2}
                              {:id 3
                               :name "Bob"}]}))))


(deftest failed-binding
  (is (not (ml/match? (ml/matcher {:x :?x :y :?x}) {:x 1 :y 2})))
  (is (not (ml/match? (ml/matcher [1 2 3]) {:x 1})))
  (is (not (ml/match? (ml/matcher {:x :?x}) {:?x 1} {:x 2}))))

(deftest failed-conjn
  (is (not (ml/match? (ml/matcher {:x :?x
                                   :y (mlcore/and :?y :?x)})
                      {:x 1
                       :y 2}))))

(deftest failed-seq
  (is (not (ml/match? (ml/matcher [1 2 3])
                      {:x 1}))))

(deftest failed-map
  (is (not (ml/match? (ml/matcher {:x 1
                                   :y 2})
                      {:x 1}))))

(deftest aggregate-rule
  (letfn [(minimum [s]
            (reify ml/Pattern
              (matches [_ preconditions n]
                (let [prev-min (get preconditions s n)]
                  (list (assoc preconditions s (min prev-min n)))))))]
    (is (= [{:?min-n 1}]
           (ml/matches (ml/matcher (repeat 3 (minimum :?min-n)))
                       [2 3 1 0])))))

(declare children)

(defn children []
  (reify ml/Pattern
    (matches [_ preconditions data]
      (ml/matches (ml/matcher
                   (mlcore/or (mlcore/scan :!path (children))
                              :?node))
                  preconditions data))))

(deftest recursive-matcher
  (is (= [{:!path [0], :?node 1}
          {:!path [1 :foo 3], :?node 3}
          {:!path [1 :foo 2], :?node 2}
          {:!path [2], :?node 3}]
         (ml/matches (ml/matcher (children)) [1 {:foo #{2 3}} 3]))))

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

(deftest matcher-as-a-function
  (let [M (ml/matcher {:id :_id
                       :messages (mlcore/scan {:author :_id
                                               :message :?msg})})]
    (is (= [{:?msg "ping"}
            {:?msg "whoohu!"}]
           (M {:id 1
               :messages [{:author 1
                           :message "ping"}
                          {:author 2
                           :message "pong"}
                          {:author 1
                           :message "whoohu!"}]})))))

(deftest and-or
  (is (= [{}] ((ml/matcher (mlcore/and 42)) 42)))
  (is (= [{}] ((ml/matcher (mlcore/or 42)) 42))))

(deftest core-predicates
  (is (ml/match? (ml/matcher (mlcore/not-eq 2)) 3))
  (is (not (ml/match? (ml/matcher (mlcore/not-eq 2)) 2)))

  (is (ml/match? (ml/matcher (mlcore/gt 2)) 3))
  (is (not (ml/match? (ml/matcher (mlcore/gt 2)) 1)))

  (is (ml/match? (ml/matcher (mlcore/gte 2)) 2))
  (is (ml/match? (ml/matcher (mlcore/gte 2)) 3))
  (is (not (ml/match? (ml/matcher (mlcore/gte 2)) 1)))

  (is (ml/match? (ml/matcher (mlcore/lt 2)) 1))
  (is (not (ml/match? (ml/matcher (mlcore/lt 2)) 3)))

  (is (ml/match? (ml/matcher (mlcore/lte 2)) 2))
  (is (ml/match? (ml/matcher (mlcore/lte 2)) 1))
  (is (not (ml/match? (ml/matcher (mlcore/lte 2)) 3)))

  (are [x y] (ml/match? (ml/matcher x) y)
    mlcore/number? 42.0
    mlcore/string? "42"
    mlcore/boolean? false
    mlcore/integer? 42
    mlcore/pos? 42
    mlcore/neg? -42
    mlcore/even? 42
    mlcore/odd? 43
    #?@(:clj [mlcore/rational? 5/3
              mlcore/decimal? 42M])
    mlcore/float? 42.0
    mlcore/double? 42.0
    mlcore/keyword? :x42
    mlcore/symbol? 'x42))

(deftest string-predicates
  (are [x y] (ml/match? (ml/matcher x) y)
    mlstring/blank? ""
    (mlstring/ends-with? "!") "qwe!"
    (mlstring/includes? "we") "qwe!"
    (mlstring/starts-with? "q") "qwe!"))
