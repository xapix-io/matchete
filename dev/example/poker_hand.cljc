(ns example.poker-hand
  (:require [matchete.core :as m]))

;; == helpers ==

(defn card-comparator [card-a card-b]
  (if (some m/pattern? [card-a card-b])
    1
    (compare card-a card-b)))

;; === rules ===

(def rules
  {'%plus (fn [s m]
            (fn [matches _ data]
              (cond
                (and (contains? matches s)
                     (= data (+ m (get matches s))))
                (list matches)

                (and (not (contains? matches s))
                     (> data m))
                (list (assoc matches s (- data m))))))})

;; =============

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
             (= (poker-hand #{[:♠ 5] [:♠ 6] [:♠ 7] [:♠ 8] [:♦ 11]})
                "Nothing")))}
  [hand]
  (letfn [(match? [pattern hand]
            (m/match? pattern rules hand))]
    (condp match? hand
      '#{[?s ?n] [?s (%plus ?n 1)] [?s (%plus ?n 2)] [?s (%plus ?n 3)] [?s (%plus ?n 4)]}
      "Straight flush"

      (sorted-set-by card-comparator '[_ ?n] '[_ ?n] '[_ ?n] '[_ ?n] '_)
      "Four of a kind"

      (sorted-set-by card-comparator '[_ ?m] '[_ ?m] '[_ ?m] '[_ ?n] '[_ ?n])
      "Full house"

      (sorted-set-by card-comparator '[?s _] '[?s _] '[?s _] '[?s _] '[?s _])
      "Flush"

      '#{[_ ?n] [_ (%plus ?n 1)] [_ (%plus ?n 2)] [_ (%plus ?n 3)] [_ (%plus ?n 4)]}
      "Straight"

      (sorted-set-by card-comparator '[_ ?n] '[_ ?n] '[_ ?n] '_ '_)
      "Three of a kind"

      (sorted-set-by card-comparator '[_ ?n] '[_ ?n] '[_ ?m] '[_ ?m] '_)
      "Two pair"

      (sorted-set-by card-comparator '[_ ?n] '[_ ?n] '_ '_ '_)
      "One pair"

      "Nothing")))
