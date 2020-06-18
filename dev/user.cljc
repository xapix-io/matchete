(ns user
  (:require [meander.epsilon :as m]
            [matchete.lang :as ml]
            [matchete.lang.core :as mlcore]
            [matchete.core :as ml2]
            [criterium.core :refer [quick-bench]]))

(defmacro predicates->pattern-defns [& preds]
  `(do ~@(for [p preds]
           `(defn ~(symbol (name p)) [& args#]
              (make-pattern ~p args#)))))

(defmacro predicates->pattern-defs [& preds]
  `(do ~@(for [p preds]
           `(def ~(symbol (name p))
              (make-pattern ~p)))))

(defn favorite-food-info [foods-by-name user]
  (m/match {:user user
            :foods-by-name foods-by-name}
    {:foods-by-name {?food {:popularity ?popularity
                            :calories ?calories}}
     :user
     {:name ?name
      :favorite-food {:name ?food}}
     }
    {:name ?name
     :favorite {:food ?food
                :popularity ?popularity
                :calories ?calories}}))

(defn favorite-foods-info [foods-by-name user]
  (m/search {:user user
             :foods-by-name foods-by-name}
    {:user
     {:name ?name
      :favorite-foods (m/scan {:name ?food})}
     :foods-by-name {?food {:popularity ?popularity
                            :calories ?calories}}}
    {:name ?name
     :favorite {:food ?food
                :popularity ?popularity
                :calories ?calories}}))

(def foods-by-name
  {:nachos {:popularity :high
            :calories :lots}
   :smoothie {:popularity :high
              :calories :less}})

;; (time
;;  (dotimes [_ 1000]
;;    (favorite-food-info foods-by-name
;;                        {:name :alice
;;                         :favorite-food {:name :nachos}})))

;; (time
;;  (dotimes [_ 1000]
;;    (favorite-foods-info foods-by-name
;;                         {:name :alice
;;                          :favorite-foods [{:name :nachos}
;;                                           {:name :smoothie}]})))

;; (let [M (ml/matcher '{:user {:name ?name
;;                              :favorite-food {:name ?food}}
;;                       :foods-by-name {?food {:popularity ?popularity
;;                                              :calories ?calories}}})]
;;   (time
;;    (dotimes [_ 1000]
;;      (M {:user {:name :alice
;;                 :favorite-food {:name :nachos}}
;;          :foods-by-name foods-by-name}))))

;; (let [M (ml/matcher {:user {:name :?name
;;                             :favorite-foods (mlcore/scan {:name :?food})}
;;                      :foods-by-name {:?food {:popularity :?popularity
;;                                              :calories :?calories}}})]
;;   (M {:user {:name :alice
;;              :favorite-foods [{:name :nachos}
;;                               {:name :smoothie}]}
;;       :foods-by-name foods-by-name})
;;   (time
;;    (dotimes [_ 1000]
;;      (M {:user {:name :alice
;;                 :favorite-foods [{:name :nachos}
;;                                  {:name :smoothie}]}
;;          :foods-by-name foods-by-name}))))

;; (m/match {:foo [1 2 3 4]}
;;   {:foo [42 '?x '?y]}
;;   {:x '?x :y '?y})

(let [M2 (ml2/matcher {:user {:name '?name
                              :favorite-foods (ml2/scan {:name '?food})}
                       :foods-by-name {'?food {:popularity '?popularity
                                               :calories '?calories}}})
      M1 (ml/matcher {:user {:name '?name
                             :favorite-foods (mlcore/scan {:name '?food})}
                      :foods-by-name {'?food {:popularity '?popularity
                                              :calories '?calories}}})]
  (prn "---DBG matchete1" (M1 {:user {:name :alice
                                      :favorite-foods [{:name :nachos}
                                                       {:name :smoothie}]}
                               :foods-by-name foods-by-name}))
  (prn "---DBG matchete2" (M2 {:user {:name :alice
                                      :favorite-foods [{:name :nachos}
                                                       {:name :smoothie}]}
                               :foods-by-name foods-by-name}))
  (prn "---DBG meander" (favorite-foods-info foods-by-name
                                             {:name :alice
                                              :favorite-foods [{:name :nachos}
                                                               {:name :smoothie}]}))
  (quick-bench (doall (M1 {:user {:name :alice
                                  :favorite-foods [{:name :nachos}
                                                   {:name :smoothie}]}
                           :foods-by-name foods-by-name})))
  (quick-bench (doall (M2 {:user {:name :alice
                                  :favorite-foods [{:name :nachos}
                                                   {:name :smoothie}]}
                           :foods-by-name foods-by-name})))
  (quick-bench (doall (favorite-foods-info foods-by-name
                                           {:name :alice
                                            :favorite-foods [{:name :nachos}
                                                             {:name :smoothie}]}))))

(let [M (ml2/matcher '{:user {:name ?name
                              :favorite-food {:name ?food}}
                       :foods-by-name {?food {:popularity ?popularity
                                              :calories ?calories}}})]
  (prn "---DBG matchete" (M {:user {:name :alice
                                    :favorite-food {:name :nachos}}
                             :foods-by-name foods-by-name}))
  (prn "---DBG meander" (favorite-food-info foods-by-name
                                            {:name :alice
                                             :favorite-food {:name :nachos}}))
  (quick-bench (doall (M {:user {:name :alice
                                 :favorite-food {:name :nachos}}
                          :foods-by-name foods-by-name})))
  (quick-bench (doall (favorite-food-info foods-by-name
                                          {:name :alice
                                           :favorite-food {:name :nachos}}))))
