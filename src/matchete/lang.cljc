(ns matchete.lang
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string])
  #?(:clj (:import (clojure.lang IFn))))

(defprotocol Pattern
  (matches [this data] [this precondition data]))

(defprotocol Matcher
  (match? [this data] [this precondition data]))

;; TODO delayed matchers
;; (defprotocol TmpValue
;;   (-value [this]))

;; (defprotocol Checks
;;   (-checks [this])
;;   (-ok? [this preconditions value]))

;; (defn add-check-fn
;;   ([f]
;;    (reify Checks
;;      (-checks [_] [f])
;;      (-ok? [this preconditions value]
;;        (filter #(not (% preconditions value)) (-checks this)))))
;;   ([checks f]
;;    (reify Checks
;;      (-checks [_] (conj (-checks checks) f))
;;      (-ok? [this preconditions value]
;;        (filter #(not (% preconditions value)) (-checks this))))))

(defn- lvar? [P]
  (and (keyword? P) (some #(string/starts-with? (name P) %) ["?" "!" "_"])))

;; (defn binding? [P]
;;   (and (keyword? P) (string/starts-with? (name P) "?")))

(defn pattern? [P]
  (or (satisfies? Pattern P)
      (lvar? P)
      (and ((some-fn map? sequential? set?) P)
           (some pattern? P))))

(declare pattern matcher)

(defn- simple-map-pattern [P]
  (let [M (reduce-kv #(assoc %1 %2 (pattern %3)) {} P)]
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
  (let [M (pattern (seq P))]
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
                                 (not (string/starts-with? (name k) key-prefix))))
                       %))
           (matches M preconditions (into {} (map (fn [v] [v v])) data))))))))

(defn- seq-pattern [PS]
  (let [MS (mapv pattern PS)]
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
    (matches [_ preconditions data]
      (if (contains? preconditions P)
        (let [val (get preconditions P)]
          (cond
            ;; TODO descide based on TmpValue
            ;; (satisfies? TmpValue val)
            ;; (when (= data (-value val))
            ;;   (list preconditions))

            ;; TODO fire all the checks associated with logical var
            ;; (satisfies? Checks val)
            ;; (if-let [pending-checks (seq (-ok? val preconditions data))]
            ;;   (list (assoc preconditions P (reify
            ;;                                  Checks
            ;;                                  (-checks [_] pending-checks)
            ;;                                  (-ok? [this preconditions value]
            ;;                                    (filter #(not (% preconditions value)) (-checks this)))
            ;;                                  TmpValue
            ;;                                  (-value [_] data))))
            ;;   (list (assoc preconditions P data)))

            (= data val)
            (list preconditions)

            :else
            ()))
        (list (assoc preconditions P data))))))

(defn- memo-binding-pattern [P]
  (reify Pattern
    (matches [_ precondition data]
      (list (update precondition P (fnil conj []) data)))))

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

(defn- clean-matches [matches]
  (reduce-kv
   (fn [m k v]
     (cond
       (= \_ (first (name k)))
       m

       ;; TODO extract value from TmpValue
       ;; (satisfies? TmpValue v)
       ;; (assoc m k (-value v))

       :else
       (assoc m k v)))
   {}
   matches))

(defn pattern [P]
  (cond
    (satisfies? Pattern P) P

    (set? P)
    (set-pattern P)

    (map? P)
    (map-pattern P)

    (sequential? P)
    (seq-pattern P)

    (lvar? P)
    (case (first (name P))
      \? (binding-pattern P)
      \! (memo-binding-pattern P)
      \_ (placeholder-pattern P))

    :else (data-pattern P)))

(defn matcher [P]
  (let [P (pattern P)]
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
        (matches P precondition data))
      IFn
      (#?(:clj invoke :cljs -invoke) [this data]
        (this {} data))
      (#?(:clj invoke :cljs -invoke) [_ preconditions data]
        (sequence (map clean-matches) (matches P preconditions data))))))
