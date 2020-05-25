(ns matchete.matcher
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
    (fn [matches scope data]
      (reduce-kv
       (fn [ms k M]
         (or (and (contains? data k)
                  (seq (mapcat #(M % scope (get data k)) ms)))
             (reduced ())))
       (list matches)
       M))))

(defn complex-map-matcher [P]
  (let [M (matcher* (seq P))]
    (fn [matches scope data]
      (when (>= (count data)
                (count P))
        (mapcat #(M matches scope %)
                (filter (fn [comb] (apply distinct? (map first comb)))
                        (combo/selections data (count P))))))))

(defn map-matcher [P]
  (let [{simple-keys false complex-keys true} (group-by binding? (keys P))
        simple-P (select-keys P simple-keys)
        simple-M (simple-map-matcher simple-P)
        complex-P (not-empty (select-keys P complex-keys))
        complex-M (when complex-P (complex-map-matcher complex-P))]
    (fn [matches scope data]
      (when (map? data)
        (let [simple-data (select-keys data simple-keys)
              complex-data (apply (partial dissoc data) simple-keys)
              matches' (simple-M matches scope simple-data)]
          (if (and complex-M (seq matches'))
            (mapcat #(complex-M % scope complex-data) matches')
            matches'))))))

(defn seq-matcher [P]
  (let [[exact-P tail-P] (split-with (partial not= '&) P)
        exact-MS (map matcher* exact-P)
        tail-M (when (seq tail-P)
                 (when-not (= 2 (count tail-P))
                   (throw (ex-info "Destructuring of a sequence tail must be a single pattern" {:pattern (rest tail-P)})))
                 (matcher* (second tail-P)))]
    (if (seq tail-P)
      (fn [matches scope data]
        (when (and (sequential? data)
                   (<= (count exact-MS) (count data)))
          (let [res (reduce
                     (fn [ms [M data]]
                       (mapcat #(M % scope data) ms))
                     (list matches)
                     (map vector exact-MS data))]
            (mapcat #(tail-M % scope (drop (count exact-MS) data)) res))))
      (fn [matches scope data]
        (when (and (sequential? data)
                   (= (count exact-MS) (count data)))
          (reduce
           (fn [ms [M data]]
             (mapcat #(M % scope data) ms))
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
          (fn [matches scope data]
            (reduce
             (fn [ms M]
               (or (seq (mapcat #(M % scope data) ms)) (reduced ())))
             (list matches)
             MS)))
        (throw (ex-info "`and` expect more than one pattern" {:pattern P})))

      or
      (if (> (count (rest P)) 1)
        (let [MS (map matcher* (rest P))]
          (fn [matches scope data]
            (reduce
             (fn [ms M]
               (if-let [ms (seq (M matches scope data))]
                 (reduced ms)
                 ms))
             ()
             MS)))
        (throw (ex-info "`or` expect more than one pattern" {:pattern P})))

      scan
      (if (= 1 (count (rest P)))
        (let [M (matcher* (second P))]
          (fn [matches scope data]
            (when (sequential? data)
              (mapcat #(M matches scope %) data))))
        (throw (ex-info "`scan` expect exactly one pattern" {:pattern P})))

      scan-indexed
      (if (= 2 (count (rest P)))
        (let [M (matcher* (rest P))]
          (fn [matches scope data]
            (cond
              (sequential? data)
              (apply concat
                     (map-indexed
                      (fn [i v]
                        (M matches scope [i v]))
                      data))

              (map? data)
              (mapcat #(M matches scope %) data))))
        (throw (ex-info "`scan-indexed` expect exactly two patterns" {:pattern P})))

      rule
      (let [M (matcher* (last P))]
        (fn f [matches scope data]
          (M matches (assoc scope (second P) f) data)))

      (seq-matcher P))

    (= '_ P)
    (fn [matches _scope _data]
      (list matches))

    (rule? P)
    (fn [matches scope data]
      (if-let [M (get scope P)]
        (M matches scope data)
        (throw (ex-info "Undefined rule" {:rule P}))))

    (binding? P)
    (fn [matches _scope data]
      (if (contains? matches P)
        (if (= data (get matches P))
          (list matches)
          ())
        (list (assoc matches P data))))

    (memo-binding? P)
    (fn [matches _scope data]
      (list (update matches P (fnil conj []) data)))

    :else
    (fn [matches _scope data]
      (if (= data P)
        (list matches)
        ()))))

(defn matcher [P]
  (let [M (matcher* P)]
    (fn f
      ([data] (f {} {} data))
      ([matches scope data]
       (set (M matches scope data))))))

(comment

  ((matcher '{"body" {"users" (scan-indexed !i {"id" ?user-id
                                                "data" !i})}
              "status" 200})
   {"body" {"users" [{"id" 1
                      "data" {"name" "Bob"}}
                     {"id" 2
                      "data" {"name" "Alise"}}]}
    "status" 200})

  ((matcher '{:body (scan-indexed ?top-level-i
                                  (scan-indexed ?inner-i
                                                (and ?user {:user-id [?top-level-i ?inner-i]})))})
   {:body [[{:user-id [0 0]}
            {:user-id [0 1]}]
           [{:user-id [1 0]}]]})

  ((matcher '(scan-indexed !path
                           (scan-indexed !path
                                         (scan-indexed !path 42))))
   {:foo {:bar {:baz 42
                :zab 24}}
    :array [{:x 1}
            {:x 42}]})

  (#_(matcher '(rule $not-leaf
                    (scan-indexed !path
                                  (or $not-leaf ?leaf))))
     (matcher '{?x !foo
                ?y ?y
                :foo :foo})
     {:foo :foo
      :x :x
      :y :y
      :z :z
      :p :o})

  )
