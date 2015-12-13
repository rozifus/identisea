(ns identisea.core
  (:gen-class)
  (:use [clojure.math.numeric-tower]
        [clojure.math.combinatorics]))

(defn result-mesh
  [functions value-sets]
  (for [f functions]
    (with-meta 
      (map (partial apply f) value-sets) 
      (meta f))))

(def add
  (with-meta
    (fn [x y] (+ x y))
    {:func "add"}))

(def mul
  (with-meta
    (fn [x y] (* x y))
    {:func "mult"}))

(def exp
  (with-meta 
    (fn [x y] (expt x y))
    {:func "exp"}))

(def modulo
  (with-meta
    (fn [x y] (mod x y))
    {:func "mod"}))

(def fermat
  (with-meta 
    (fn [x y] (modulo (exp x y) y))
    {:func "fermat"}))

(def functions [add mul exp modulo fermat])

(def search-size 500)

(def value-sets
  (for [x (range 2 search-size)
        y (range 2 search-size)]
    [x y]))

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
    (for [[f1 f2] (combinations mesh 2)]
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

(defn -main
  [& args]
  (main)) 


