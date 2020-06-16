(ns example.poker-hand
  (:require [matchete.lang :as ml]))

(defn card [P]
  (ml/matcher P))

(defn hand-pattern [pattern]
  (ml/matcher (into #{} (map card) pattern)))

(defn match? [matcher hand]
  (ml/match? matcher hand))

(defn p+ [lvar n]
  (reify ml/Pattern
    (matches [_ preconditions m]
      (cond
        (and (contains? preconditions lvar)
             (= m (+ n (get preconditions lvar))))
        (list preconditions)

        (and (not (contains? preconditions lvar))
             (> m n))
        (list (assoc preconditions lvar (- m n)))))))

(defn high-card-> [lvar]
  (reify ml/Pattern
    (matches [_ {[_ rank' :as card'] lvar} [_ rank :as card]]
      (list {lvar (cond
                    (nil? card')
                    card

                    (> rank rank')
                    card

                    :else
                    card')}))))

(let [p (hand-pattern [[:?s 14] [:?s 13] [:?s 12] [:?s 11] [:?s 10]])]
  (defn royal-flush? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [[:?s :?n] [:?s (p+ :?n 1)] [:?s (p+ :?n 2)] [:?s (p+ :?n 3)] [:?s (p+ :?n 4)]])]
  (defn straight-flush? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [[:_ :?n] [:_ :?n] [:_ :?n] [:_ :?n] :_])]
  (defn four-of-a-kind? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [[:_ :?m] [:_ :?m] [:_ :?m] [:_ :?n] [:_ :?n]])]
  (defn full-house? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [[:?s :_] [:?s :_] [:?s :_] [:?s :_] [:?s :_]])]
  (defn flush? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [[:_ :?n] [:_ (p+ :?n 1)] [:_ (p+ :?n 2)] [:_ (p+ :?n 3)] [:_ (p+ :?n 4)]])]
  (defn straight? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [[:_ :?n] [:_ :?n] [:_ :?n] :_ :_])]
  (defn three-of-a-kind? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [[:_ :?n] [:_ :?n] [:_ :?m] [:_ :?m] :_])]
  (defn two-pair? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [[:_ :?n] [:_ :?n] :_ :_ :_])]
  (defn one-pair? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern (repeatedly 5 #(high-card-> :?card)))]
  (defn high-card [hand]
    (:?card (first (ml/matches p hand)))))

(defn poker-hand
  {:test #(do
            (assert
             (= (poker-hand #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 8] [:♠ 9]})
                "Straight flush"))
            (assert
             (= (poker-hand #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 5]})
                "Four of a kind"))
            (assert
             (= (poker-hand #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 7]})
                "Full house"))
            (assert
             (= (poker-hand #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 13] [:♠ 9]})
                "Flush"))
            (assert
             (= (poker-hand #{[:♠ 5] [:♣ 6] [:♠ 7] [:♠ 8] [:♠ 9]})
                "Straight"))
            (assert
             (= (poker-hand #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 8]})
                "Three of a kind"))
            (assert
             (= (poker-hand #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 10]})
                "Two pair"))
            (assert
             (= (poker-hand #{[:♠ 5] [:♦ 10] [:♠ 7] [:♣ 5] [:♥ 8]})
                "One pair"))
            (assert
             (= (poker-hand #{[:♠ 5] [:♦ 11] [:♠ 6] [:♠ 7] [:♠ 8]})
                [:♦ 11])))}
  [hand]
  (cond
    (royal-flush? hand)
    "Royal flush"

    (straight-flush? hand)
    "Straight flush"

    (four-of-a-kind? hand)
    "Four of a kind"

    (full-house? hand)
    "Full house"

    (flush? hand)
    "Flush"

    (straight? hand)
    "Straight"

    (three-of-a-kind? hand)
    "Three of a kind"

    (two-pair? hand)
    "Two pair"

    (one-pair? hand)
    "One pair"

    :else
    (high-card hand)))
