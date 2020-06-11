(ns matchete.logic
  (:refer-clojure :exclude [not conj disj var?])
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [#?(:clj clojure.core :cljs cljs.core) :as cc]))

(defn- var? [P]
  (and (keyword? P) (some #(string/starts-with? (name P) %) ["?" "!" "_"])))

(defprotocol Pattern
  (matches [this data] [this precondition data]))

(defprotocol Matcher
  (match? [this data] [this precondition data]))

(defn pattern? [P]
  (or (satisfies? Pattern P)
      (var? P)
      (and ((some-fn map? sequential? set?) P)
           (some pattern? P))))

(declare matcher pattern*)

(defn conj
  "conj[unction] of multiple patterns."
  [& patterns]
  (let [MS (mapv pattern* patterns)]
    (reify Pattern
      (matches [_ precondition data]
        (reduce
         (fn [ms M]
           (or
            (seq (mapcat #(matches M % data) ms))
            (reduced ())))
         (list precondition)
         MS)))))

(defn disj
  "disj[unction] of multiple patterns."
  [& patterns]
  (let [MS (mapv pattern* patterns)]
    (reify Pattern
      (matches [_ precondition data]
        (reduce
         (fn [ms M]
           (if-let [ms (seq (matches M precondition data))]
             (reduced ms)
             ms))
         ()
         MS)))))

(defn not [pattern]
  (let [M (matcher pattern)]
    (reify Pattern
      (matches [_ preconditions data]
        (when-not (match? M preconditions data)
          (list preconditions))))))

(defn each
  ([item-pattern]
   (let [M (pattern* item-pattern)]
     (reify Pattern
       (matches [_ preconditions data]
         (when (sequential? data)
           (reduce
            (fn [ms [M data]]
              (mapcat #(matches M % data) ms))
            (list preconditions)
            (map vector (repeat (count data) M) data)))))))
  ([index-pattern item-pattern]
   (let [M (each [index-pattern item-pattern])]
     (reify Pattern
       (matches [_ preconditions data]
         (matches M preconditions (map-indexed vector data)))))))

(defn scan
  ([item-pattern]
   (let [M (pattern* item-pattern)]
     (reify Pattern
       (matches [_ preconditions data]
         (when ((some-fn sequential? map? set?) data)
           (mapcat #(matches M preconditions %) data))))))
  ([index-pattern item-pattern]
   (let [M (pattern* [index-pattern item-pattern])]
     (reify Pattern
       (matches [_ preconditions data]
         (when ((some-fn sequential? map? set?) data)
           (cond
             (sequential? data)
             (apply concat
                    (map-indexed
                     (fn [i v]
                       (matches M preconditions [i v]))
                     data))

             (map? data)
             (mapcat (fn [[k v]] (matches M preconditions [k v])) data)

             (set? data)
             (mapcat (fn [v] (matches M preconditions [v v])) data))))))))

(defn- simple-map-pattern [P]
  (let [M (reduce-kv #(assoc %1 %2 (pattern* %3)) {} P)]
    (reify Pattern
      (matches [_ preconditions data]
        (reduce-kv
         (fn [ms k M]
           (or (and (contains? data k)
                    (seq (mapcat #(matches M % (get data k)) ms)))
               (reduced ())))
         (list preconditions)
         M)))))

(defn- complex-map-pattern [P]
  (let [M (pattern* (seq P))]
    (reify Pattern
      (matches [_ preconditions data]
        (when (>= (count data)
                  (count P))
          (mapcat #(matches M preconditions %)
                  (filter (fn [comb] (apply distinct? (map first comb)))
                          (combo/selections data (count P)))))))))

(defn- map-pattern [P]
  (let [{simple-keys false complex-keys true} (group-by pattern? (keys P))
        simple-P (select-keys P simple-keys)
        simple-M (simple-map-pattern simple-P)
        complex-P (not-empty (select-keys P complex-keys))
        complex-M (when complex-P (complex-map-pattern complex-P))]
    (reify Pattern
      (matches [_ preconditions data]
        (when (map? data)
          (let [simple-data (select-keys data simple-keys)
                complex-data (apply (partial dissoc data) simple-keys)
                preconditions' (matches simple-M preconditions simple-data)]
            (if (and complex-M (seq preconditions'))
              (mapcat #(matches complex-M % complex-data) preconditions')
              preconditions')))))))

(defn- set->map-pattern [prefix P]
  (let [{simple false
         complex true} (group-by pattern? P)]
    (merge
     (into {} (map (fn [v] [v v])) simple)
     (into {} (map (fn [v] [(keyword (gensym prefix)) v])) complex))))

(defn- set-pattern [P]
  (let [key-prefix (str (name (gensym "_")) "_")
        M (map-pattern (set->map-pattern key-prefix P))]
    (reify Pattern
      (matches [_ preconditions data]
        (when (set? data)
          (sequence
           (map #(into {}
                       (filter (fn [[k _]]
                                 (cc/not (string/starts-with? (name k) key-prefix))))
                       %))
           (matches M preconditions (into {} (map (fn [v] [v v])) data))))))))

(defn- seq-pattern [patterns-list]
  (let [MS (mapv pattern* patterns-list)]
    (reify Pattern
      (matches [_ preconditions data]
        (when (and (sequential? data)
                   (<= (count MS) (count data)))
          (reduce-kv
           (fn [preconditions M d]
             (mapcat #(matches M % d) preconditions))
           (list preconditions)
           (zipmap MS data)))))))

(defn- binding-pattern [P]
  (reify Pattern
    (matches [_ precondition data]
      (if (contains? precondition P)
        (if (= data (get precondition P))
          (list precondition)
          ())
        (list (assoc precondition P data))))))

(defn- memo-binding-pattern [P]
  (reify Pattern
    (matches [_ precondition data]
      (list (update precondition P (fnil cc/conj []) data)))))

(defn- placeholder-pattern [P]
  (if (> (count (name P)) 1)
    (binding-pattern P)
    (reify Pattern
      (matches [_ precondition _data]
        (list precondition)))))

(defn- data-pattern [value]
  (reify
    Pattern
    (matches [_ precondition data]
      (when (= data value)
        (list precondition)))))

(defn- pattern* [P]
  (cond
    (satisfies? Pattern P) P

    (set? P)
    (set-pattern P)

    (map? P)
    (map-pattern P)

    (sequential? P)
    (seq-pattern P)

    (var? P)
    (case (first (name P))
      \? (binding-pattern P)
      \! (memo-binding-pattern P)
      \_ (placeholder-pattern P))

    :else (data-pattern P)))

(defn clean-matches [matches]
  (reduce-kv
   (fn [m k v]
     (if (= \_ (first (name k)))
       m
       (assoc m k v)))
   {}
   matches))

(defn matcher [pattern]
  (let [P (pattern* pattern)]
    (reify
      Matcher
      (match? [this data]
        (match? this {} data))
      (match? [this precondition data]
        (boolean (seq (matches this precondition data))))
      Pattern
      (matches [this data]
        (matches this {} data))
      (matches [this precondition data]
        (matches P precondition data)))))
