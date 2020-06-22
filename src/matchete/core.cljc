(ns matchete.core
  (:refer-clojure :rename {some core-some
                           and core-and
                           or core-or}
                  :exclude [not])
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn lvar? [x]
  (core-and (simple-symbol? x) (= \? (first (name x)))))

(defn placeholder? [x]
  (core-and (lvar? x) (= \_ (second (name x)))))

(defn pattern? [P]
  (core-or (:pattern (meta P))
           (lvar? P)
           (core-and ((some-fn map? sequential? set?) P)
                     (core-some pattern? P))))

(declare pattern)

(defn- simple-map-pattern [P]
  (let [P (reduce-kv #(assoc %1 %2 (pattern %3)) {} P)]
    (fn [data ms]
      (reduce-kv
       (fn [ms k v]
         (core-or (seq (v (get data k) ms)) (reduced ())))
       ms
       P))))

(defn- complex-map-pattern [P]
  (if (empty? P)
    (fn [_data ms] ms)
    (let [M (pattern (seq P))]
      (fn [data ms]
        (mapcat #(M % ms)
                (filter #(apply distinct? (map first %))
                        (combo/selections data (count P))))))))

(defn map-pattern [P]
  (let [{scalar-keys false variable-keys true} (group-by pattern? (keys P))
        simple-pattern (simple-map-pattern (select-keys P scalar-keys))
        complex-pattern (complex-map-pattern (select-keys P variable-keys))]
    (with-meta
      (fn [data ms]
        (when (core-and (map? data)
                        (>= (count data)
                            (count P))
                        (every? #(contains? data %) scalar-keys))
          (let [simple-data (select-keys data scalar-keys)
                rest-data (apply dissoc data scalar-keys)]
            (when-let [ms' (seq (simple-pattern simple-data ms))]
              (complex-pattern rest-data ms')))))
      {:pattern true})))

(defn- seq-pattern [P]
  (let [[P [_ TP :as tail]] (split-with #(not= '& %) P)
        P (mapv pattern P)
        TP (when (seq tail) (pattern TP))]
    (with-meta
      (fn [data ms]
        (when (core-and (sequential? data)
                        (>= (count data) (count P)))
          (reduce
           (fn [ms [P data]]
             (core-or (seq (P data ms)) (reduced ())))
           ms
           (concat (partition 2 (interleave P data))
                   (when TP (list [TP (drop (count P) data)]))))))
      {:pattern true})))

(defn- complex-set-pattern [P]
  (if (empty? P)
    (fn [_data ms] ms)
    (let [M (pattern (seq P))]
      (fn [data ms]
        (mapcat #(M % ms)
                (filter #(apply distinct? %)
                        (combo/selections data (count P))))))))

(defn- set-pattern [P]
  (let [{scalar-items false variable-items true} (group-by pattern? P)
        scalar-items (set scalar-items)
        M (complex-set-pattern variable-items)]
    (with-meta
      (fn [data ms]
        (when (core-and (set? data)
                        (>= (count data)
                            (count P))
                        (set/subset? scalar-items data))
          (M (set/difference data scalar-items) ms)))
      {:pattern true})))

(defn- lvar-pattern [P]
  (with-meta
    (fn [data ms]
      (sequence
       (comp
        (remove #(core-and (contains? % P) (not= data (get % P))))
        (map (fn [m]
               (core-or (core-and (contains? m P) m)
                        (let [{::keys [guards] :as m} (assoc m P data)]
                          (core-or (core-and (empty? guards) m)
                                   (reduce
                                    (fn [m guard]
                                      (case (guard m)
                                        true m
                                        false (reduced nil)
                                        (update m ::guards conj guard)))
                                    (assoc m ::guards [])
                                    guards))))))
        (filter some?))
       ms))
    {:pattern true}))

(defn- placeholder-pattern [P]
  (if (> (count (name P)) 2)
    (lvar-pattern P)
    (with-meta
      (fn [_data ms] ms)
      {:pattern true})))

(defn- data-pattern [P]
  (with-meta
    (fn [data ms]
      (when (= data P) ms))
    {:pattern true}))

(defn pattern [P]
  (cond
    (:pattern (meta P))
    P

    (map? P)
    (map-pattern P)

    (set? P)
    (set-pattern P)

    (sequential? P)
    (seq-pattern P)

    (placeholder? P)
    (placeholder-pattern P)

    (lvar? P)
    (lvar-pattern P)

    :else
    (data-pattern P)))

(def memoized-pattern (memoize pattern))

(defn scan
  ([P]
   (let [M (pattern P)]
     (with-meta
       (fn [data ms]
         (when ((some-fn sequential? map?) data)
           (mapcat #(M % ms) data)))
       {:pattern true})))
  ([index-P value-P]
   (let [M (pattern [index-P value-P])]
     (with-meta
       (fn [data ms]
         (cond
           (map? data)
           (mapcat #(M [% (get data %)] ms) (keys data))

           (sequential? data)
           (apply concat (map-indexed #(M [%1 %2] ms) data))))
       {:pattern true}))))

(defn each [P]
  (let [M (pattern P)]
    (with-meta
      (fn [data ms]
        (when (sequential? data)
          (reduce
           (fn [ms item]
             (core-or (seq (M item ms)) (reduced ())))
           ms
           data)))
      {:pattern true})))

(defn some [P]
  (let [M (pattern P)]
    (with-meta
      (fn [data ms]
        (when (sequential? data)
          (let [[found-one? ms]
                (reduce
                 (fn [[found-one? ms] item]
                   (if-let [ms' (seq (M item ms))]
                     [true ms']
                     [found-one? ms]))
                 [false ms]
                 data)]
            (if found-one? ms ()))))
      {:pattern true})))

(defn and
  ([P] (pattern P))
  ([P & PS]
   (let [MS (mapv pattern (list* P PS))]
     (with-meta
       (fn [data ms]
         (reduce
          (fn [ms M]
            (core-or (seq (M data ms)) (reduced ())))
          ms
          MS))
       {:pattern true}))))

(defn or
  ([P] (pattern P))
  ([P & PS]
   (let [MS (mapv pattern (list* P PS))]
     (with-meta
       (fn [data ms]
         (reduce
          (fn [ms' M]
            (if-let [ms' (seq (M data ms))]
              (reduced ms')
              ms'))
          ()
          MS))
       {:pattern true}))))

(defn not [P]
  (let [M (pattern P)]
    (with-meta
      (fn [data ms]
        (when-not (seq (M data ms))
          ms))
      {:pattern true})))

(defn lif
  ([cond-pattern then-pattern]
   (lif cond-pattern then-pattern ::empty))
  ([cond-pattern then-pattern else-pattern]
   (let [cond-m (pattern cond-pattern)
         then-m (pattern then-pattern)
         else-m (when (not= ::empty else-pattern)
                  (pattern else-pattern))]
     (with-meta
       (fn [data ms]
         (if-let [ms' (seq (cond-m data ms))]
           (then-m data ms')
           (when else-m (else-m data ms))))
       {:pattern true}))))

(defn predicate
  ([pred]
   (predicate pred nil))
  ([pred dest]
   (with-meta
     (fn [data ms]
       (when (pred data)
         (core-or
          (core-and (some? dest)
                    (sequence
                     (map #(assoc % dest data))
                     ms))
          ms)))
     {:pattern true})))

(defn aggregate-by
  ([aggr-fn]
   (with-meta
     (fn [data ms]
       (sequence
        (comp
         (map #(aggr-fn % data))
         (filter some?))
        ms))
     {:pattern true}))
  ([aggr-fn dest]
   (aggregate-by #(update %1 dest aggr-fn %2))))

(defn reshape-by [tr-fn P]
  (let [M (pattern P)]
    (with-meta
      (fn [data ms]
        (M (tr-fn data) ms))
      {:pattern true})))

(defn- find-lvars [expr]
  (cond
    (coll? expr)
    (set (mapcat find-lvars expr))

    (lvar? expr)
    (list expr)

    :else
    nil))

(defn check-guard [guard ms]
  (sequence
   (comp
    (map (fn [m]
           (case (guard m)
             true m
             false nil
             (update m ::guards (fnil conj []) guard))))
    (filter some?))
   ms))

(defmacro formula [expr & [dest]]
  (let [m (gensym)
        lvars (find-lvars expr)]
    `(and
      (with-meta
        (fn [data# ms#]
          (letfn [(f# [~m]
                    (if (every? #(contains? ~m %) ~(mapv (fn [s] `(symbol ~(name s))) lvars))
                      (let [{:syms ~(vec lvars) :as x#} ~m]
                        (= data# ~expr))
                      f#))]
            (check-guard f# ms#)))
        {:pattern true})
      ~@(if dest `((symbol ~(name dest))) ()))))

(defmacro defpattern [name P]
  `(def ~name
     (with-meta
       (fn [data# ms#]
         ((memoized-pattern ~P) data# ms#))
       {:pattern true})))

(defmacro defnpattern [name args P]
  `(def ~name
     (with-meta
       (fn [data# ms#]
         (sequence
          (mapcat (fn [m#]
                    (let [P# (memoized-pattern ((fn ~args ~P) m#))]
                      (P# data# [m#]))))
          ms#))
       {:pattern true})))

(defn result-of [f & [dest]]
  (let [lvars (:lvars (meta f))
        guard (with-meta
                (fn [data ms]
                  (letfn [(f' [m]
                            (if (every? #(contains? m %) lvars)
                              (= data (f m))
                              f'))]
                    (check-guard f' ms)))
                {:pattern true})]
    (if dest
      (and guard dest)
      guard)))

(defn matcher [P]
  (let [M (pattern P)]
    (with-meta
      (fn f
        ([data] (f data {}))
        ([data preconditions]
         (sequence
          (comp
           (remove #(not-empty (::guards %)))
           (map #(dissoc % ::guards)))
          (M data [preconditions]))))
      {:matcher true})))

(defn match?
  ([M data] (match? M {} data))
  ([M preconditions data]
   (boolean (seq ((if (:matcher (meta M)) M (matcher M)) data preconditions)))))

(defn matches
  ([M data] (matches M {} data))
  ([M preconditions data]
   (sequence
    (map (fn [m]
           (into {} (remove #(placeholder? (first %))) m)))
    ((if (:matcher (meta M)) M (matcher M)) data preconditions))))
