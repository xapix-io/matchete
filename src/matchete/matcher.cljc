(ns matchete.matcher
  (:require [clojure.math.combinatorics :as combo]))

(defn binding? [P]
  (and (simple-symbol? P)
       (= \? (first (name P)))))

(defn memo-binding? [P]
  (and (simple-symbol? P)
       (= \! (first (name P)))))

(defn find-bindings [P]
  (cond
    ((some-fn binding? memo-binding?) P)
    (list P)

    (sequential? P)
    (mapcat find-bindings P)

    (map? P)
    (mapcat find-bindings (seq P))

    :else ()))

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
  (let [{simple-keys false complex-keys true} (group-by binding? (keys P))
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

(defn seq-matcher [P]
  (let [[exact-P tail-P] (split-with (partial not= '&) P)
        exact-MS (map matcher* exact-P)
        tail-M (when (seq tail-P)
                 (when-not (= 2 (count tail-P))
                   (throw (ex-info "Destructuring of a sequence tail must be a single pattern" {:pattern (rest tail-P)})))
                 (matcher* (second tail-P)))]
    (if (seq tail-P)
      (fn [matches data]
        (when (and (sequential? data)
                   (<= (count exact-MS) (count data)))
          (let [res (reduce
                     (fn [ms [M data]]
                       (mapcat #(M % data) ms))
                     (list matches)
                     (map vector exact-MS data))]
            (mapcat #(tail-M % (drop (count exact-MS) data)) res))))
      (fn [matches data]
        (when (and (sequential? data)
                   (= (count exact-MS) (count data)))
          (reduce
           (fn [ms [M data]]
             (mapcat #(M % data) ms))
           (list matches)
           (map vector exact-MS data)))))))

(defn matcher* [P]
  (cond
    (map? P)
    (map-matcher P)

    (sequential? P)
    (case (first P)
      and
      (if (> (count (rest P)) 1)
        (let [MS (map matcher* (rest P))]
          (fn [matches data]
            (reduce
             (fn [ms M]
               (or (seq (mapcat #(M % data) ms)) (reduced ())))
             (list matches)
             MS)))
        (throw (ex-info "`and` expect more than one pattern" {:pattern P})))

      or
      (if (> (count (rest P)) 1)
        (let [MS (map matcher* (rest P))]
          (fn [matches data]
            (reduce
             (fn [ms M]
               (if-let [ms (seq (mapcat #(M % data) ms))]
                 (reduced ms)
                 ms))
             (list matches)
             MS)))
        (throw (ex-info "`or` expect more than one pattern" {:pattern P})))

      scan
      (if (= 1 (count (rest P)))
        (let [M (matcher* (second P))]
          (fn [matches data]
            (when (sequential? data)
              (mapcat #(M matches %) data))))
        (throw (ex-info "`scan` expect exactly one pattern" {:pattern P})))

      (seq-matcher P))

    (= '_ P)
    (fn [matches _data]
      (list matches))

    (binding? P)
    (fn [matches data]
      (if (contains? matches P)
        (if (= data (get matches P))
          (list matches)
          ())
        (list (assoc matches P data))))

    (memo-binding? P)
    (fn [matches data]
      (list (update matches P (fnil conj []) data)))

    :else
    (fn [matches data]
      (if (= data P)
        (list matches)
        ()))))

(defn matcher [P]
  (let [M (matcher* P)]
    (fn f
      ([data] (f {} data))
      ([matches data]
       (set (M matches data))))))
