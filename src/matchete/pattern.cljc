(ns matchete.pattern
  (:require [clojure.math.combinatorics :as combo]))

(defn- binding? [P]
  (and (symbol? P)
       (= (first (name P)) \?)))

(defn- memo-binding? [P]
  (and (symbol? P)
       (= (first (name P)) \!)))

(defn- placeholder? [P]
  (= '_ P))

(defn- pattern? [P]
  (or (binding? P)
      (and (sequential? P)
           (contains? #{'cat 'alt 'exact} (first P)))))

(declare matcher*)

(defn simple-map-matcher [P]
  (let [M (reduce-kv #(assoc %1 %2 (matcher* %3)) {} P)]
    (fn [matches data]
      (reduce-kv
       (fn [ms k M]
         (or (and (contains? data k)
                  (seq (mapcat #(M % (get data k)) ms)))
             (reduced ())))
       (list matches)
       M))))

(defn complex-map-matcher [P]
  (let [M (matcher* (seq P))]
    (fn [matches data]
      (when (>= (count data)
                (count P))
        (mapcat #(M matches %)
                (filter (fn [comb] (apply distinct? (map first comb)))
                        (combo/selections data (count P))))))))

(defn map-matcher [P]
  (let [{simple-keys false
         complex-keys true}
        (group-by pattern? (keys P))
        simple-P (select-keys P simple-keys)
        simple-M (simple-map-matcher simple-P)
        complex-P (not-empty (select-keys P complex-keys))
        complex-M (when complex-P (complex-map-matcher complex-P))]
    (fn [matches data]
      (when (map? data)
        (let [simple-data (select-keys data simple-keys)
              complex-data (apply (partial dissoc data) simple-keys)
              matches' (simple-M matches simple-data)]
          (if (and complex-M (seq matches'))
            (mapcat #(complex-M % complex-data) matches')
            matches'))))))

(defn seq-matcher [P exact?]
  (case (first P)
    cat (let [MS (map matcher* (rest P))]
          (fn [matches data]
            (reduce
             (fn [ms M]
               (or (seq (mapcat #(M % data) ms)) (reduced ())))
             (list matches)
             MS)))
    alt (let [MS (map matcher* (rest P))]
          (fn [matches data]
            (reduce
             (fn [ms M]
               (if-let [ms (seq (mapcat #(M % data) ms))]
                 (reduced ms)
                 ms))
             (list matches)
             MS)))
    exact (seq-matcher (second P) true)
    (let [MS (map matcher* P)]
      (fn [matches data]
        (when (and (sequential? data)
                   ((if exact? = <=)
                    (count MS)
                    (count data)))
          (reduce
           (fn [ms [M data]]
             (mapcat #(M % data) ms))
           (list matches)
           (map vector MS data)))))))

(defn matcher* [P]
  (cond
    (map? P)
    (map-matcher P)

    (sequential? P)
    (seq-matcher P false)

    (placeholder? P)
    (fn [matches _data]
      (list matches))

    (memo-binding? P)
    (fn [matches data]
      (list (update matches P (fnil conj []) data)))

    (binding? P)
    (fn [matches data]
      (if (contains? matches P)
        (or (and (= data (matches P)) (list matches)) ())
        (list (assoc matches P data))))

    :else
    (fn [matches data]
      (if (= data P)
        (list matches)
        ()))))

(defn matcher [P]
  (let [M (matcher* P)]
    (fn f
      ([data] (f data {}))
      ([data matches]
       (M matches data)))))

(defn matches [P data]
  ((matcher P) data))

(defn match? [P data]
  (boolean (seq (matches P data))))
