(ns matchete.lang.core
  (:refer-clojure :exclude [every? some number? string? boolean? integer?
                            pos? neg? even? odd? rational? decimal? float? double?
                            keyword? symbol?
                            or and])
  (:require [matchete.lang :as ml]
            [clojure.core :as cc]))

(defn- make-pattern [p]
  (reify ml/Pattern
    (matches [_ preconditions data]
      (when (p data)
        (list preconditions)))))

(defn and
  ([P] (ml/pattern P))
  ([P & patterns]
   (let [MS (mapv ml/pattern (list* P patterns))]
     (reify ml/Pattern
       (matches [_ precondition data]
         (reduce
          (fn [ms M]
            (cc/or (seq (mapcat #(ml/matches M % data) ms))
                   (reduced ())))
          (list precondition)
          MS))))))

(defn or
  ([P] (ml/pattern P))
  ([P & patterns]
   (let [MS (mapv ml/pattern (list* P patterns))]
     (reify ml/Pattern
       (matches [_ precondition data]
         (reduce
          (fn [ms M]
            (if-let [ms (seq (ml/matches M precondition data))]
              (reduced ms)
              ms))
          ()
          MS))))))

(defn every?
  ([item-pattern]
   (let [M (ml/pattern item-pattern)]
     (reify ml/Pattern
       (matches [_ preconditions data]
         (when (sequential? data)
           (reduce
            (fn [ms data]
              (mapcat #(ml/matches M % data) ms))
            (list preconditions)
            data))))))
  ([index-pattern item-pattern]
   (let [M (every? [index-pattern item-pattern])]
     (reify ml/Pattern
       (matches [_ preconditions data]
         (when (sequential? data)
           (ml/matches M preconditions (map-indexed vector data))))))))

(defn some
  ([item-pattern]
   (let [M (ml/pattern item-pattern)]
     (reify ml/Pattern
       (matches [_ preconditions data]
         (when (sequential? data)
           (reduce
            (fn [ms data]
              (if-let [ms' (seq (mapcat #(ml/matches M % data) ms))]
                ms' ms))
            (list preconditions)
            data))))))
  ([index-pattern item-pattern]
   (let [M (some [index-pattern item-pattern])]
     (reify ml/Pattern
       (matches [_ preconditions data]
         (when (sequential? data)
           (ml/matches M preconditions (map-indexed vector data))))))))

(defn scan
  ([item-pattern]
   (let [M (ml/pattern item-pattern)]
     (reify ml/Pattern
       (matches [_ preconditions data]
         (when ((some-fn sequential? map? set?) data)
           (mapcat #(ml/matches M preconditions %) data))))))
  ([index-pattern item-pattern]
   (let [M (ml/pattern [index-pattern item-pattern])]
     (reify ml/Pattern
       (matches [_ preconditions data]
         (when ((some-fn sequential? map? set?) data)
           (cond
             (sequential? data)
             (apply concat
                    (map-indexed
                     (fn [i v]
                       (ml/matches M preconditions [i v]))
                     data))

             (map? data)
             (mapcat (fn [[k v]] (ml/matches M preconditions [k v])) data)

             (set? data)
             (mapcat (fn [v] (ml/matches M preconditions [v v])) data))))))))

(def number? (make-pattern cc/number?))
(def string? (make-pattern cc/string?))
(def boolean? (make-pattern cc/boolean?))
(def integer? (make-pattern cc/integer?))
(def pos? (make-pattern cc/pos?))
(def neg? (make-pattern cc/neg?))
(def even? (make-pattern cc/even?))
(def odd? (make-pattern cc/odd?))
(def rational? (make-pattern cc/rational?))
(def decimal? (make-pattern cc/decimal?))
(def float? (make-pattern cc/float?))
(def double? (make-pattern cc/double?))
(def keyword? (make-pattern cc/keyword?))
(def symbol? (make-pattern cc/symbol?))

;; (defmulti compare-pattern #(ml/binding? %2))

;; (defmethod compare-pattern true [f lvar]
;;   (reify ml/Pattern
;;     (matches [_ p data]
;;       (cond
;;         (satisfies? ml/Checks (get p lvar))
;;         (list (assoc p lvar (ml/add-check-fn (get p lvar)
;;                                              (fn [_ value]
;;                                                (f data value)))))

;;         (contains? p lvar)
;;         (when (f data (get p lvar)) (list p))

;;         :else
;;         (list (assoc p lvar (ml/add-check-fn
;;                              (fn [_ lvar-data]
;;                                (f data lvar-data)))))))))

;; (defmethod compare-pattern false [f val]
;;   (reify ml/Pattern
;;     (matches [_ p data]
;;       (when (f data val)
;;         (list p)))))

(defn compare-pattern [f val]
  (reify ml/Pattern
    (matches [_ p data]
      (when (f data val)
        (list p)))))

(defn not-eq [lvar]
  (compare-pattern not= lvar))

(defn gt [lvar]
  (compare-pattern > lvar))

(defn gte [lvar]
  (compare-pattern >= lvar))

(defn lt [lvar]
  (compare-pattern < lvar))

(defn lte [lvar]
  (compare-pattern <= lvar))
