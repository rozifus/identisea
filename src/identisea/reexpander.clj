(ns identisea.reexpander
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

(def variables 2)

(defprotocol Operation
  (operate [this t p])
  (last-op [this]))

(def r-add
  (for [i (range variables)]
    (reify Operation
      (operate [this t p] (+ t (nth p i)))
      (last-op [this] "r-add"))))

(def varbase 
  (for [i (range variables)]
    (with-meta (fn [p] (nth p i))
               {:math :var})))

(def t-add 
  (fn [i o]
    (with-meta (fn [p] (+ (i p) (o p))) 
               {:math :lin})))

(def t-multiply 
  (fn [i o]
    (with-meta (fn [p] (* (i p) (o p)))
               {:math :geo})))

(def t-exp
  (fn [i o]
    (with-meta (fn [p] (math/expt (i p) (o p)))
               {:math :exp})))

(def exp-t
  (fn [i o]
    (with-meta (fn [p] (math/expt (o p) (i p)))
               {:math :exp})))

(def t-mod
  (fn [i o]
    (with-meta (fn [p] (mod (i p) (o p)))
               {:math :mod})))

(def mod-t
  (fn [i o]
    (with-meta (fn [p] (mod (o p) (i p)))
               {:math :mod})))

#_(def fermat
  (with-meta 
    (fn [x y] (x-mod-y (x-exp-y x y) y))
    {:func "fermat"}))

(def functions varbase)

(def operations [t-add t-multiply t-exp exp-t t-mod mod-t])

#_(def functions
  (map (fn [f] (vary-meta f assoc :last (:math (meta f)))) functions))

(def complexity 2)
(def const-min 2)
(def const-max 50)

(defn non-redundant-composition 
  [o f1 f2]
  (and (not= (:math (meta o)) (:math (meta f1)))
       (not= (:math (meta o)) (:math (meta f2)))))

(defn compose-functions 
  [o f1 f2]
  (with-meta (o f1 f2) (meta o)))

(doseq [i (range complexity)]
  (def functions
    (into functions
      (for [o operations
            f1 functions
            f2 functions
            :when (non-redundant-composition o f1 f2)] 
        (compose-functions o f1 f2)))))

(doseq [f functions]
  (println f))

(def search-size 10)

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


(def mesh
  (for [p (comb/combinations (range 2 search-size) variables)]
    (for [f functions]
      (f p))))

(defn main []
  (doall (map println 
              (map format-function-pair-result
                   (search-mesh mesh =)))))


