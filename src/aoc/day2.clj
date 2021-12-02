(ns aoc.day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))






(defn solve
  [in]
  (let [data (->> (-> (utils/load-text-input in)
                      (str/split-lines))
                  (map #(str/split % #" "))
                  (map #(vector (keyword (first %)) (edn/read-string (second %)))))]
    (loop [cur (first data)
           rem (rest data)
           x 0
           y 0]
      (if cur
        (let [[dir dist] cur]
          (case dir
            :forward
            (recur (first rem) (rest rem) (+ x dist) y)
            :up
            (recur (first rem) (rest rem) x (- y dist))
            :down
            (recur (first rem) (rest rem) x (+ dist y))
            :error))
        (* x y))))
  )

(defn solve-v2
  [in]
  (let [data (->> (-> (utils/load-text-input in)
                         (str/split-lines))
             (map #(str/split % #" "))
             (map #(vector (keyword (first %)) (edn/read-string (second %)))))]
    (loop [cur (first data)
           rem (rest data)
           x 0
           y 0
           aim 0]
      (if cur
        (let [[dir dist] cur]
          (case dir
            :forward
            (recur (first rem) (rest rem) (+ x dist) (+ y (* aim dist)) aim)
            :up
            (recur (first rem) (rest rem) x y (- aim dist))
            :down
            (recur (first rem) (rest rem) x y (+ aim dist))
            :error))
        (* x y))))
)


(comment
  (solve "day2.txt")
  (solve-v2 "day2.txt")
  )
