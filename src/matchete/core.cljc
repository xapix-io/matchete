(ns matchete.core
  (:refer-clojure :exclude [not conj disj])
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [#?(:clj clojure.core :cljs cljs.core) :as cc]))

;; TODO extend matcher's metadata with to-edn function

(declare matcher* match?)

(defn conj
  "conj[unction] of multiple patterns."
  [& PS]
  (let [MS (map matcher* PS)]
    (with-meta
      (fn [matches data]
        (reduce
         (fn [ms M]
           (or (seq (mapcat #(M % data) ms)) (reduced ())))
         (list matches)
         MS))
      {::matcher? true})))

(defn disj
  "disj[unction] of multiple patterns."
  [& PS]
  (let [MS (map matcher* PS)]
    (with-meta
      (fn [matches data]
        (reduce
         (fn [ms M]
           (if-let [ms (seq (M matches data))]
             (reduced ms)
             ms))
         ()
         MS))
      {::matcher? true})))

(defn not [P]
  (let [M (matcher* P)]
    (with-meta
      (fn [matches data]
        (when-not (match? M matches data)
          (list matches)))
      {::matcher? true})))

(defn each
  ([P]
   (let [M (matcher* P)]
     (with-meta
       (fn [matches data]
         (when (sequential? data)
           (reduce
            (fn [ms [M data]]
              (mapcat #(M % data) ms))
            (list matches)
            (map vector (repeat (count data) M) data))))
       {::matcher? true})))
  ([index-P value-P]
   (let [M (each [index-P value-P])]
     (with-meta
       (fn [matches data]
         (M matches (map-indexed vector data)))
       {::matcher? true}))))

(defn scan
  ([P]
   (let [M (matcher* P)]
     (with-meta
       (fn [matches data]
         (when (sequential? data)
           (mapcat #(M matches %) data)))
       {::matcher? true})))
  ([index-P value-P]
   (let [M (matcher* [index-P value-P])]
     (with-meta
       (fn [matches data]
         (when (sequential? data)
           (apply concat
                  (map-indexed
                   (fn [i v]
                     (M matches [i v]))
                   data))))
       {::matcher? true}))))

(defn- logic-var? [P]
  (and (keyword? P) (some #(string/starts-with? (name P) %) ["?" "!" "_"])))

(defn pattern? [P]
  (or (logic-var? P)
      ((some-fn ::matcher? ::matcher-maker?) (meta P))
      (and ((some-fn map? sequential? set?) P)
           (some pattern? P))))

(defn- binding-matcher [P]
  (with-meta
    (fn [matches data]
      (if (contains? matches P)
        (if (= data (get matches P))
          (list matches)
          ())
        (list (assoc matches P data))))
    {::matcher? true}))

(defn- memo-binding-matcher [P]
  (with-meta
    (fn [matches data]
      (list (update matches P (fnil cc/conj []) data)))
    {::matcher? true}))

(defn- placeholder-matcher [P]
  (if (> (count (name P)) 1)
    (binding-matcher P)
    (with-meta
      (fn [matches _data]
        (list matches))
      {::matcher? true})))

(defn- data-matcher [D]
  (with-meta
    (fn [matches data]
      (if (= data D)
        (list matches)
        ()))
    {::matcher? true}))

(defn- seq-matcher [PS]
  (let [MS (map matcher* PS)]
    (with-meta
      (fn [matches data]
        (when (and (sequential? data)
                   (<= (count MS) (count data)))
          (reduce-kv
           (fn [matches M d]
             (mapcat #(M % d) matches))
           (list matches)
           (zipmap MS data))))
      {::matcher? true})))

(defn- simple-map-matcher [P]
  (let [M (reduce-kv #(assoc %1 %2 (matcher* %3)) {} P)]
    (with-meta
      (fn [matches data]
        (reduce-kv
         (fn [ms k M]
           (or (and (contains? data k)
                    (seq (mapcat #(M % (get data k)) ms)))
               (reduced ())))
         (list matches)
         M))
      {::matcher? true})))

(defn- complex-map-matcher [P]
  (let [M (matcher* (seq P))]
    (with-meta
      (fn [matches data]
        (when (>= (count data)
                  (count P))
          (mapcat #(M matches %)
                  (filter (fn [comb] (apply distinct? (map first comb)))
                          (combo/selections data (count P))))))
      {::matcher? true})))

(defn- map-matcher [P]
  (let [{simple-keys false complex-keys true} (group-by pattern? (keys P))
        simple-P (select-keys P simple-keys)
        simple-M (simple-map-matcher simple-P)
        complex-P (not-empty (select-keys P complex-keys))
        complex-M (when complex-P (complex-map-matcher complex-P))]
    (with-meta
      (fn [matches data]
        (when (map? data)
          (let [simple-data (select-keys data simple-keys)
                complex-data (apply (partial dissoc data) simple-keys)
                matches' (simple-M matches simple-data)]
            (if (and complex-M (seq matches'))
              (mapcat #(complex-M % complex-data) matches')
              matches'))))
      {::matcher? true})))

(defn- set->map-pattern [P]
  (let [{simple false
         complex true} (group-by pattern? P)]
    (merge
     (into {} (map (fn [v] [v v])) simple)
     (into {} (map (fn [v] [(keyword (gensym "_")) v])) complex))))

(defn- set-matcher [P]
  (let [m (set->map-pattern P)
        M (map-matcher m)]
    (with-meta
      (fn [matches data]
        (when (set? data)
          (M matches (into {} (map (fn [v] [v v])) data))))
      {::matcher? true})))

(defn- matcher* [P]
  (cond
    (::matcher? (meta P))
    P

    (set? P)
    (set-matcher P)

    (map? P)
    (map-matcher P)

    (sequential? P)
    (seq-matcher P)

    (logic-var? P)
    (case (first (name P))
      \? (binding-matcher P)
      \! (memo-binding-matcher P)
      \_ (placeholder-matcher P))

    :else
    (data-matcher P)))

(defn clean-matches [matches]
  (reduce-kv
   (fn [m k v]
     (if (= \_ (first (name k)))
       m
       (assoc m k v)))
   {}
   matches))

(defn matcher [P]
  (let [M (matcher* P)]
    (with-meta
      (fn f
        ([data] (f {} data))
        ([matches data]
         (sequence
          (map clean-matches)
          (M matches data))))
      {::matcher? true})))

(defn matches
  ([pattern data] (matches pattern {} data))
  ([pattern init-matches data]
   (sequence
    (map clean-matches)
    (if (fn? pattern)
      (pattern init-matches data)
      ((matcher pattern) init-matches data)))))

(defn match?
  ([pattern data] (match? pattern {} data))
  ([pattern init-matches data]
   (boolean (seq (matches pattern init-matches data)))))
