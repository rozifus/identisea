(ns identisea.core
  (:gen-class)
  (:use [clojure.math.numeric-tower]))

(defn result-mesh
  [functions value-sets]
  (for [f functions]
    (map (partial apply f) value-sets)))

(defn add 
  [x y]
  (+ x y))

(defn mul
  [x y]
  (* x y))

(defn exp
  [x y]
  (expt x y))

(defn modulo
  [x y]
  (mod x y))

(defn fermat [x y]
  (modulo (exp x y) y))

(def functions [add mul exp modulo fermat])

(def search-size 400)

(def value-sets
  (for [x (range 1 search-size)
        y (range 1 search-size)]
    [x y]))

(defn decimal-true
  [values]
  (float (/ (count (filter identity values))
            (count values)))) 

(defn search-mesh
  [mesh function]
    (for [f1 mesh 
          f2 mesh
          :when (not= f1 f2)]
      (decimal-true (map function f1 f2))))

(defn main []
  (let [rm (result-mesh functions value-sets)]
    (doall (map println rm))
    (doall (map println (search-mesh rm =)))))

(defn -main
  [& args]
  (main)) 


