(ns aoc.day1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))


(defn solve
  [in]
  (let [raw (utils/load-edn-input in)]
    (loop [current (second raw)
           prev     (first raw)
           remaining (drop 2 raw)
           up-cnt 0
           down-cnt 0]
      (if current  
        (cond
          (< current prev)
          (recur (first remaining) current (rest remaining) up-cnt (inc down-cnt))
          
          (> current prev)
          (recur (first remaining) current (rest remaining) (inc up-cnt) down-cnt))
          
        up-cnt))))


(defn solve-v2
  [in]
  (let [raw (utils/load-edn-input in)]
    (loop [current (second raw)
           prev     (first raw)
           remaining (drop 2 raw)
           up-cnt 0
           down-cnt 0]
      (if current  
        (cond
          (< current prev)
          (recur (first remaining) current (rest remaining) up-cnt (inc down-cnt))
          
          (> current prev)
          (recur (first remaining) current (rest remaining) (inc up-cnt) down-cnt))
          
        up-cnt))))

(comment
  (solve-v2 "day1.edn")
  )
