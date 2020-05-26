(ns matchete.core
  (:require [clojure.math.combinatorics :as combo]))

(defn binding? [P]
  (and (simple-symbol? P)
       (= \? (first (name P)))))

(defn memo-binding? [P]
  (and (simple-symbol? P)
       (= \! (first (name P)))))

(defn rule? [P]
  (and (simple-symbol? P)
       (= \$ (first (name P)))))

(declare matcher*)

(defn simple-map-matcher [P]
  (let [M (reduce-kv #(assoc %1 %2 (matcher* %3)) {} P)]
    (fn [matches rules data]
      (reduce-kv
       (fn [ms k M]
         (or (and (contains? data k)
                  (seq (mapcat #(M % rules (get data k)) ms)))
             (reduced ())))
       (list matches)
       M))))

(defn complex-map-matcher [P]
  (let [M (matcher* (seq P))]
    (fn [matches rules data]
      (when (>= (count data)
                (count P))
        (mapcat #(M matches rules %)
                (filter (fn [comb] (apply distinct? (map first comb)))
                        (combo/selections data (count P))))))))

(defn map-matcher [P]
  (let [{simple-keys false complex-keys true} (group-by binding? (keys P))
        simple-P (select-keys P simple-keys)
        simple-M (simple-map-matcher simple-P)
        complex-P (not-empty (select-keys P complex-keys))
        complex-M (when complex-P (complex-map-matcher complex-P))]
    (fn [matches rules data]
      (when (map? data)
        (let [simple-data (select-keys data simple-keys)
              complex-data (apply (partial dissoc data) simple-keys)
              matches' (simple-M matches rules simple-data)]
          (if (and complex-M (seq matches'))
            (mapcat #(complex-M % rules complex-data) matches')
            matches'))))))

(defn seq-matcher [P]
  (let [[exact-P tail-P] (split-with (partial not= '&) P)
        exact-MS (map matcher* exact-P)
        tail-M (when (seq tail-P)
                 (when-not (= 2 (count tail-P))
                   (throw (ex-info "Destructuring of a sequence tail must be a single pattern" {:pattern (rest tail-P)})))
                 (matcher* (second tail-P)))]
    (if (seq tail-P)
      (fn [matches rules data]
        (when (and (sequential? data)
                   (<= (count exact-MS) (count data)))
          (let [res (reduce
                     (fn [ms [M data]]
                       (mapcat #(M % rules data) ms))
                     (list matches)
                     (map vector exact-MS data))]
            (mapcat #(tail-M % rules (drop (count exact-MS) data)) res))))
      (fn [matches rules data]
        (when (and (sequential? data)
                   (= (count exact-MS) (count data)))
          (reduce
           (fn [ms [M data]]
             (mapcat #(M % rules data) ms))
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
          (fn [matches rules data]
            (reduce
             (fn [ms M]
               (or (seq (mapcat #(M % rules data) ms)) (reduced ())))
             (list matches)
             MS)))
        (throw (ex-info "`and` expect more than one pattern" {:pattern P})))

      or
      (if (> (count (rest P)) 1)
        (let [MS (map matcher* (rest P))]
          (fn [matches rules data]
            (reduce
             (fn [ms M]
               (if-let [ms (seq (M matches rules data))]
                 (reduced ms)
                 ms))
             ()
             MS)))
        (throw (ex-info "`or` expect more than one pattern" {:pattern P})))

      scan
      (if (= 1 (count (rest P)))
        (let [M (matcher* (second P))]
          (fn [matches rules data]
            (when (or (sequential? data)
                      (map? data))
              (mapcat #(M matches rules %) data))))
        (throw (ex-info "`scan` expect exactly one pattern" {:pattern P})))

      scan-indexed
      (if (= 2 (count (rest P)))
        (let [M (matcher* (rest P))]
          (fn [matches rules data]
            (cond
              (sequential? data)
              (apply concat
                     (map-indexed
                      (fn [i v]
                        (M matches rules [i v]))
                      data))

              (map? data)
              (mapcat #(M matches rules %) data))))
        (throw (ex-info "`scan-indexed` expect exactly two patterns" {:pattern P})))

      rule
      (let [M (matcher* (last P))]
        (fn f [matches rules data]
          (M matches (assoc rules (second P) f) data)))

      (seq-matcher P))

    (= '_ P)
    (fn [matches _rules _data]
      (list matches))

    (rule? P)
    (fn [matches rules data]
      (if-let [M (get rules P)]
        (M matches rules data)
        (throw (ex-info "Undefined rule" {:rule P}))))

    (binding? P)
    (fn [matches _rules data]
      (if (contains? matches P)
        (if (= data (get matches P))
          (list matches)
          ())
        (list (assoc matches P data))))

    (memo-binding? P)
    (fn [matches _rules data]
      (list (update matches P (fnil conj []) data)))

    :else
    (fn [matches _rules data]
      (if (= data P)
        (list matches)
        ()))))

(defn matcher [P]
  (let [M (matcher* P)]
    (fn f
      ([data] (f {} {} data))
      ([matches data] (f matches {} data))
      ([matches rules data]
       (M matches rules data)))))

(defn matches
  ([pattern data] (matches pattern {} {} data))
  ([pattern rules data] (matches pattern {} rules data))
  ([pattern init-matches rules data]
   (if (fn? pattern)
     (pattern init-matches rules data)
     ((matcher pattern) init-matches rules data))))

(defn match?
  ([pattern data] (match? pattern {} {} data))
  ([pattern rules data] (match? pattern {} rules data))
  ([pattern init-matches rules data]
   (boolean (seq (matches pattern init-matches rules data)))))
