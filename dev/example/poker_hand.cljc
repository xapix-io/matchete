(ns example.poker-hand
  (:require [matchete.core :as m]))

;; == helpers ==

(defn match-n-plus-m [m]
  (fn [{:syms [?n] :as matches} _ data]
    (cond
      (and ?n (= data (+ ?n m)))
      (list matches)

      (and (nil? ?n) (> data m))
      (list (assoc matches '?n (- data m))))))

(defn card-comparator [card-a card-b]
  (if (some m/pattern? [card-a card-b])
    1
    (compare card-a card-b)))

;; === rules ===

(def rules
  {'$n   (match-n-plus-m 0)
   '$n+1 (match-n-plus-m 1)
   '$n+2 (match-n-plus-m 2)
   '$n+3 (match-n-plus-m 3)
   '$n+4 (match-n-plus-m 4)})

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
      '#{[?s $n] [?s $n+1] [?s $n+2] [?s $n+3] [?s $n+4]}
      "Straight flush"

      (sorted-set-by card-comparator '[_ ?n] '[_ ?n] '[_ ?n] '[_ ?n] '_)
      "Four of a kind"

      (sorted-set-by card-comparator '[_ ?m] '[_ ?m] '[_ ?m] '[_ ?n] '[_ ?n])
      "Full house"

      (sorted-set-by card-comparator '[?s _] '[?s _] '[?s _] '[?s _] '[?s _])
      "Flush"

      '#{[_ $n] [_ $n+1] [_ $n+2] [_ $n+3] [_ $n+4]}
      "Straight"

      (sorted-set-by card-comparator '[_ ?n] '[_ ?n] '[_ ?n] '_ '_)
      "Three of a kind"

      (sorted-set-by card-comparator '[_ ?n] '[_ ?n] '[_ ?m] '[_ ?m] '_)
      "Two pair"

      (sorted-set-by card-comparator '[_ ?n] '[_ ?n] '_ '_ '_)
      "One pair"

      "Nothing")))
