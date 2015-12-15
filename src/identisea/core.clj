(ns identisea.core
  (:gen-class)
  (:require [identisea.reexpander]))

(defn -main
  [& args]
  (identisea.reexpander/main)) 


