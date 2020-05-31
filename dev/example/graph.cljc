(ns example.graph
  (:require [matchete.core :as m]))

(def city-to-city-distance
  '#{["Berlin" #{("New York" 14) ("London" 2) ("Tokyo" 14) ("Vancouver" 13)}]
     ["New York" #{("Berlin" 14) ("London" 12) ("Tokyo" 18) ("Vancouver" 6)}]
     ["London" #{("Berlin" 2) ("New York" 12) ("Tokyo" 15) ("Vancouver" 10)}]
     ["Tokyo" #{("Berlin" 14) ("New York" 18) ("London" 15) ("Vancouver" 12)}]
     ["Vancouver" #{("Berlin" 13) ("New York" 6) ("London" 10) ("Tokyo" 12)}]})

;; generates pattern like this:
#_'#{[(cat ?0 !path) #{[?1 $sum]}]
     [(cat ?1 !path) #{[?2 $sum]}]
     [(cat ?2 !path) #{[?3 $sum]}]
     [(cat ?3 !path) #{[?4 $sum]}]
     [(cat ?4 !path) #{[?0 $sum]}]}
(defn generate-pattern [cities-count]
  (let [l (range cities-count)]
    (into #{}
          (map (fn [[n1 n2]]
                 [(list 'cat (symbol (str "?" n1)) '!path)
                  #{[(symbol (str "?" n2)) '$sum]}]))
          (take cities-count (map vector (cycle l) (rest (cycle l)))))))

(defn shortest-path [db]
  (let [{:syms [?distance !path]}
        (first
         (sort-by #(get % '?distance)
                  (m/matches (generate-pattern (count db))
                             {'?0 (ffirst db)}
                             ;; Let's use rule as a reduce to calculate a distance walked so far
                             {'$sum (fn [matches _rules data]
                                      (list (update matches '?distance (fnil + 0) data)))}
                             db)))]
    [?distance !path]))

(comment

  (shortest-path city-to-city-distance)

  )
