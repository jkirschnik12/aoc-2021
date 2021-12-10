(ns aoc.day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))




(def t-in [16,1,2,0,4,2,7,1,2,14])

(defn move-cost
  [distance]
  (/ (* distance (inc distance)) 2))

(defn calc-fuel
  [crabs origin]
  (->> crabs
       #_(map (memoize #(apply + (range (inc (Math/abs (- origin %)))))))
       (map (memoize move-cost))
       (apply +)))

(defn solve
  [in]
  (let [crabs (utils/load-edn-input in)
        lower-bound (apply min crabs)
        upper-bound (apply max crabs)
        mem-fuel (memoize (fn [crabs origin]
                            (calc-fuel crabs origin)))]
    (apply min (reduce (fn [acc val]
               (conj acc (mem-fuel crabs val))) [] (range lower-bound upper-bound))))

  )





(comment
  (time (solve "day7.edn"))
  (solve-v2 "day6.edn")
  )
