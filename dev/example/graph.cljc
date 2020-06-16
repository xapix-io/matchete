(ns example.graph
  (:require [matchete.lang :as ml]))

(def city-to-city-distance
  #{["Berlin" #{["New York" 14] ["London" 2] ["Tokyo" 14] ["Vancouver" 13]}]
    ["New York" #{["Berlin" 14] ["London" 12] ["Tokyo" 18] ["Vancouver" 6]}]
    ["London" #{["Berlin" 2] ["New York" 12] ["Tokyo" 15] ["Vancouver" 10]}]
    ["Tokyo" #{["Berlin" 14] ["New York" 18] ["London" 15] ["Vancouver" 12]}]
    ["Vancouver" #{["Berlin" 13] ["New York" 6] ["London" 10] ["Tokyo" 12]}]})

(def calculate-distance
  (reify ml/Pattern
    (matches [_ preconditions data]
      (list (update preconditions :?distance (fnil + 0) data)))))

(defn generate-matcher [cities-count]
  (let [l (range cities-count)]
    (ml/matcher
     (into #{}
           (map (fn [[n1 n2]]
                  [(keyword (str "?" n1))
                   #{[(keyword (str "?" n2)) calculate-distance]}]))
           (take cities-count (map vector (cycle l) (rest (cycle l))))))))

(defn shortest-path
  {:test #(do
            (assert
             (= 46 (shortest-path city-to-city-distance "Berlin"))))}
  [db start]
  (let [{:keys [?distance]}
        (first
         (sort-by :?distance
                  (ml/matches (generate-matcher (count db))
                              {:?0 start}
                              db)))]
    ?distance))
