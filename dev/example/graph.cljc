(ns example.graph
  (:require [matchete.core :as ml]))

(def city-to-city-distance
  #{["Berlin" #{["New York" 14] ["London" 2] ["Tokyo" 14] ["Vancouver" 13]}]
    ["New York" #{["Berlin" 14] ["London" 12] ["Tokyo" 18] ["Vancouver" 6]}]
    ["London" #{["Berlin" 2] ["New York" 12] ["Tokyo" 15] ["Vancouver" 10]}]
    ["Tokyo" #{["Berlin" 14] ["New York" 18] ["London" 15] ["Vancouver" 12]}]
    ["Vancouver" #{["Berlin" 13] ["New York" 6] ["London" 10] ["Tokyo" 12]}]})

(defn add-distance [distance path]
  (+ (or distance 0) path))

(defn generate-matcher [cities-count]
  (let [l (range cities-count)]
    (into #{}
          (map (fn [[n1 n2]]
                 [(symbol (str "?" n1))
                  #{[(symbol (str "?" n2)) (ml/aggregate add-distance '?distance)]}]))
          (take cities-count (map vector (cycle l) (rest (cycle l)))))))

(defn shortest-path
  {:test #(do
            (assert
             (= 46 (shortest-path city-to-city-distance "Berlin"))))}
  [db start]
  (let [{:syms [?distance]}
        (first
         (sort-by #(get % '?distance)
                  (ml/matches (generate-matcher (count db))
                              {'?0 start}
                              db)))]
    ?distance))

(comment

  (shortest-path city-to-city-distance "Berlin")

  )
