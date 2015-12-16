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
               {:math :var :str (str "#" i)})))

; should use a macro to create operators 
(def t-add 
  (with-meta (fn [i o] 
               (fn [p] (+ (i p) (o p)))) 
             {:math :lin :fmt "(+ %1$s %2$s)"}))

(def t-multiply 
  (with-meta (fn [i o] 
               (fn [p] (* (i p) (o p)))) 
             {:math :geo :fmt "(* %1$s %2$s)"}))

(def t-exp
  (with-meta (fn [i o] 
               (fn [p] (math/expt (i p) (o p)))) 
             {:math :exp :fmt "(exp %1$s %2$s)"}))

(def exp-t
  (with-meta (fn [i o] 
               (fn [p] (math/expt (o p) (i p)))) 
             {:math :exp :fmt "(exp %1$s %2$s)"}))

(def t-mod
  (with-meta (fn [i o] 
               (fn [p] (mod (i p) (o p)))) 
             {:math :mod :fmt "(mod %1$s %2$s)"}))

(def mod-t
  (with-meta (fn [i o] 
               (fn [p] (mod (o p) (i p)))) 
             {:math :mod :fmt "(mod %1$s %2$s)"}))

#_(def fermat
  (with-meta 
    (fn [x y] (x-mod-y (x-exp-y x y) y))
    {:func "fermat"}))

(def functions varbase)

(def operations [t-add t-multiply t-exp t-mod])

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
  (let [mo (meta o)
        srep (format (:fmt mo) (:str (meta f1)) (:str (meta f2)))]
    (with-meta (o f1 f2) (assoc mo :str srep)))) 

(doseq [i (range complexity)]
  (def functions
    (into functions
      (for [o operations
            f1 functions
            f2 functions
            :when (non-redundant-composition o f1 f2)] 
        (compose-functions o f1 f2)))))

(doseq [f functions]
  (println (:str (meta f))))

(def search-size 10)

(defn decimal-true
  [values]
  (let [cutoff (/ 100.0 search-size)
        valid (count (filter identity values))
        result (* 100 (float (/ valid (count values))))]
    (if (> result cutoff)
      result
      0.0)))

(defn format-equality
  [f1 f2]
  (format "%s = %s" (:str (meta f1)) (:str (meta f2))))

(defn search-mesh
  [mesh comparison]
    (for [[f1 f2] (comb/combinations mesh 2)]
      (with-meta [(decimal-true (map comparison f1 f2))]
                 {:str (format-equality f1 f2)})))

(defn format-function-pair-result
  [meta-val]
    (str meta-val " " (:str (meta meta-val)) "-" (:str (meta meta-val))))


(def mesh
  (for [p (comb/combinations (range 2 search-size) variables)]
    (for [f functions]
      (f p))))

(defn main []
  (doall (map #(println (str % " " (:str (meta %)))) 
              (search-mesh mesh =))))


