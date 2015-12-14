(ns identisea.expander
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as comb]
            [serializable.fn :as sfn]))

(defn result-mesh
  [functions value-sets]
  (for [f functions]
    (with-meta 
      (map (partial apply f) value-sets) 
      (meta f))))

(def x-add-y 
  (with-meta
    (sfn/fn [x y] (+ x y))
    {:func "x-add-y"}))

(def x-mult-y 
  (with-meta
    (sfn/fn [x y] (* x y))
    {:func "x-mult-y"}))

(def x-exp-y
  (with-meta 
    (sfn/fn [x y] (math/expt x y))
    {:func "x-exp-y"}))

(def x-mod-y 
  (with-meta
    (sfn/fn [x y] (mod x y))
    {:func "x-mod-y"}))

(def fermat
  (with-meta 
    (sfn/fn [x y] (x-mod-y (x-exp-y x y) y))
    {:func "fermat"}))

(def functions [x-add-y x-mult-y x-exp-y x-mod-y])
(def functions
  (map (fn [f] (vary-meta f assoc :last (:func (meta f)))) functions))
(def functions
  (map (fn [f] (vary-meta f assoc :path (:func (meta f)))) functions))

(def complexity 2)
(def const-min 2)
(def const-max 50)

(doseq [i (range complexity)]
  (def functions
    (into functions
      (for [f1 functions
            f2 functions
            :when (not= (:last (meta f1)) (:last (meta f2)))] 
        (with-meta (comp f1 f2) 
                   (assoc (meta f1) 
                          :path (str (:path (meta f1)) ":" 
                                     (:path (meta f2) ))))))))

(doseq [f functions]
  (println f))

(def search-size 10)

(def value-sets
  (for [a (range 2 search-size)
        b (range 2 search-size)]
    [(fn [x y] a) 
     (fn [x y] b)]))

(defn decimal-true
  [values]
  (let [cutoff (/ 100.0 search-size)
        valid (count (filter identity values))
        result (* 100 (float (/ valid (count values))))]
    (if (> result cutoff)
      result
      0.0)))

(defn search-mesh
  [mesh comparison]
    (for [[f1 f2] (comb/combinations mesh 2)]
      (with-meta [(decimal-true (map comparison f1 f2))]
                 {:f1 (:func (meta f1)) :f2 (:func (meta f2)) :comp "="})))

(defn format-function-pair-result
  [meta-val]
    (str meta-val " " (:f1 (meta meta-val)) "-" (:f2 (meta meta-val))))

(defn main []
  (let [rm (result-mesh functions value-sets)]
    (doall (map println 
                (map format-function-pair-result
                     (search-mesh rm =))))))



