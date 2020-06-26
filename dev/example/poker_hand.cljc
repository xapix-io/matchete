(ns example.poker-hand
  (:require [matchete.core :as ml :include-macros true]))

(defn card [P]
  (ml/pattern P))

(defn hand-pattern [pattern]
  (ml/matcher (into #{} (map card) pattern)))

(defn match? [matcher hand]
  (ml/match? matcher hand))

(let [p (hand-pattern '[[?s 14] [?s 13] [?s 12] [?s 11] [?s 10]])]
  (defn royal-flush? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [['?s '?n]
                       ['?s (ml/formula (+ ?n 1))]
                       ['?s (ml/formula (+ ?n 2))]
                       ['?s (ml/formula (+ ?n 3))]
                       ['?s (ml/formula (+ ?n 4))]])]
  (defn straight-flush? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern '[[?_ ?n]
                        [?_ ?n]
                        [?_ ?n]
                        [?_ ?n]
                        ?_])]
  (defn four-of-a-kind? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern '[[?_ ?m]
                        [?_ ?m]
                        [?_ ?m]
                        [?_ ?n]
                        [?_ ?n]])]
  (defn full-house? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern '[[?s ?_]
                        [?s ?_]
                        [?s ?_]
                        [?s ?_]
                        [?s ?_]])]
  (defn flush? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern [['?_ '?n]
                       ['?_ (ml/formula (+ ?n 1))]
                       ['?_ (ml/formula (+ ?n 2))]
                       ['?_ (ml/formula (+ ?n 3))]
                       ['?_ (ml/formula (+ ?n 4))]])]
  (defn straight? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern '[[?_ ?n]
                        [?_ ?n]
                        [?_ ?n]
                        ?_
                        ?_])]
  (defn three-of-a-kind? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern '[[?_ ?n]
                        [?_ ?n]
                        [?_ ?m]
                        [?_ ?m]
                        ?_])]
  (defn two-pair? [hand]
    (ml/match? p hand)))

(let [p (hand-pattern '[[?_ ?n]
                        [?_ ?n]
                        ?_
                        ?_
                        ?_])]
  (defn one-pair? [hand]
    (ml/match? p hand)))

(let [aggr-fn (fn [[_ old-rank :as old-card] [_ new-rank :as new-card]]
                (if (> new-rank (or old-rank 0))
                  new-card
                  old-card))
      p (hand-pattern [(ml/aggregate-by aggr-fn :high-card)
                       (ml/aggregate-by aggr-fn :high-card)
                       (ml/aggregate-by aggr-fn :high-card)
                       (ml/aggregate-by aggr-fn :high-card)
                       (ml/aggregate-by aggr-fn :high-card)])]
  (defn high-card [hand]
    (:high-card (first (ml/matches p hand)))))

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

(comment

  (time
   (dotimes [_ 10]
     (test #'poker-hand)))

  (royal-flush? #{[:♠ 10] [:♠ 11] [:♠ 12] [:♠ 13] [:♠ 9]})

  (straight-flush? #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 8] [:♠ 9]})

  (straight? #{[:♠ 5] [:♣ 6] [:♠ 7] [:♠ 8] [:♠ 9]})

  (high-card #{[:♠ 5] [:♦ 5] [:♠ 7] [:♣ 5] [:♥ 5]})

  )
