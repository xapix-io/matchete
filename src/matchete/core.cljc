(ns matchete.core
  (:refer-clojure :exclude [cat])
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

(defn- simple-map-matcher [P]
  (let [M (reduce-kv #(assoc %1 %2 (matcher* %3)) {} P)]
    ^::matcher?
    (fn [matches rules data]
      (reduce-kv
       (fn [ms k M]
         (or (and (contains? data k)
                  (seq (mapcat #(M % rules (get data k)) ms)))
             (reduced ())))
       (list matches)
       M))))

(defn- complex-map-matcher [P]
  (let [M (matcher* (seq P))]
    ^::matcher?
    (fn [matches rules data]
      (when (>= (count data)
                (count P))
        (mapcat #(M matches rules %)
                (filter (fn [comb] (apply distinct? (map first comb)))
                        (combo/selections data (count P))))))))

(defn- map-matcher [P]
  (let [{simple-keys false complex-keys true} (group-by (some-fn binding? memo-binding?) (keys P))
        simple-P (select-keys P simple-keys)
        simple-M (simple-map-matcher simple-P)
        complex-P (not-empty (select-keys P complex-keys))
        complex-M (when complex-P (complex-map-matcher complex-P))]
    ^::matcher?
    (fn [matches rules data]
      (when (map? data)
        (let [simple-data (select-keys data simple-keys)
              complex-data (apply (partial dissoc data) simple-keys)
              matches' (simple-M matches rules simple-data)]
          (if (and complex-M (seq matches'))
            (mapcat #(complex-M % rules complex-data) matches')
            matches'))))))

(defn- seq-matcher [P]
  (let [[exact-P tail-P] (split-with (partial not= '&) P)
        exact-MS (map matcher* exact-P)
        tail-M (when (seq tail-P)
                 (when-not (= 2 (count tail-P))
                   (throw (ex-info "Destructuring of a sequence tail must be a single pattern" {:pattern (rest tail-P)})))
                 (matcher* (second tail-P)))]
    (if (seq tail-P)
      ^::matcher?
      (fn [matches rules data]
        (when (and (sequential? data)
                   (<= (count exact-MS) (count data)))
          (let [res (reduce
                     (fn [ms [M data]]
                       (mapcat #(M % rules data) ms))
                     (list matches)
                     (map vector exact-MS data))]
            (mapcat #(tail-M % rules (drop (count exact-MS) data)) res))))
      ^::matcher?
      (fn [matches rules data]
        (when (and (sequential? data)
                   (= (count exact-MS) (count data)))
          (reduce
           (fn [ms [M data]]
             (mapcat #(M % rules data) ms))
           (list matches)
           (map vector exact-MS data)))))))

(defn- set->map [s]
  (into {} (map #(vec (repeat 2 %))) s))

(defn- set-matcher [P]
  (let [M (matcher* (set->map P))]
    ^::matcher?
    (fn [matches rules data]
      (when (set? data)
        (M matches rules (set->map data))))))

(defn cat [& PS]
  (let [MS (map matcher* PS)]
    ^::matcher?
    (fn [matches rules data]
      (reduce
       (fn [ms M]
         (or (seq (mapcat #(M % rules data) ms)) (reduced ())))
       (list matches)
       MS))))

(defn alt [& PS]
  (let [MS (map matcher* PS)]
    ^::matcher?
    (fn [matches rules data]
      (reduce
       (fn [ms M]
         (if-let [ms (seq (M matches rules data))]
           (reduced ms)
           ms))
       ()
       MS))))

(defn scan [P]
  (let [M (matcher* P)]
    ^::matcher?
    (fn [matches rules data]
      (when (or (sequential? data)
                (map? data))
        (mapcat #(M matches rules %) data)))))

(defn scan-indexed [index-P value-P]
  (let [M (matcher* [index-P value-P])]
    ^::matcher?
    (fn [matches rules data]
      (cond
        (sequential? data)
        (apply concat
               (map-indexed
                (fn [i v]
                  (M matches rules [i v]))
                data))

        (map? data)
        (mapcat #(M matches rules %) data)))))

(defn def-rule [name P]
  (let [M (matcher* P)]
    ^::matcher?
    (fn f [matches rules data]
      (M matches (assoc rules name f) data))))

(defn- matcher* [P]
  (cond
    (and (fn? P)
         (::matcher? (meta P)))
    P

    (set? P)
    (set-matcher P)

    (map? P)
    (map-matcher P)

    (sequential? P)
    (case (first P)
      cat
      (apply cat (rest P))

      alt
      (apply alt (rest P))

      scan
      (if (= 1 (count (rest P)))
        (scan (second P))
        (throw (ex-info (str "Wrong number of args (" (count (rest P)) ") passed to: scan") {:pattern P})))

      scan-indexed
      (if (= 2 (count (rest P)))
        (apply scan-indexed (rest P))
        (throw (ex-info (str "Wrong number of args (" (count (rest P)) ") passed to: scan-indexed") {:pattern P})))

      def-rule
      (apply def-rule (rest P))

      (seq-matcher P))

    (= '_ P)
    ^::matcher?
    (fn [matches _rules _data]
      (list matches))

    (rule? P)
    ^::matcher?
    (fn [matches rules data]
      (if-let [M (get rules P)]
        (M matches rules data)
        (throw (ex-info "Undefined rule" {:rule P}))))

    (binding? P)
    ^::matcher?
    (fn [matches _rules data]
      (if (contains? matches P)
        (if (= data (get matches P))
          (list matches)
          ())
        (list (assoc matches P data))))

    (memo-binding? P)
    ^::matcher?
    (fn [matches _rules data]
      (list (update matches P (fnil conj []) data)))

    :else
    ^::matcher?
    (fn [matches _rules data]
      (if (= data P)
        (list matches)
        ()))))

(defn matcher [P]
  (let [M (matcher* P)]
    ^::matcher?
    (fn f
      ([data] (f {} {} data))
      ([matches data] (f matches {} data))
      ([matches rules data]
       (M matches rules data)))))

(defn matches
  ([pattern data] (matches pattern {} {} data))
  ([pattern rules data] (matches pattern {} rules data))
  ([pattern init-matches rules data]
   (let [rules (reduce-kv #(assoc %1 %2 (if (fn? %3) %3 (matcher %3))) {} rules)]
     (if (fn? pattern)
       (pattern init-matches rules data)
       ((matcher pattern) init-matches rules data)))))

(defn match?
  ([pattern data] (match? pattern {} {} data))
  ([pattern rules data] (match? pattern {} rules data))
  ([pattern init-matches rules data]
   (boolean (seq (matches pattern init-matches rules data)))))
