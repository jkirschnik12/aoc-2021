(ns aoc.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))

(defn third
  [coll]
  (get coll 2 nil))

(defn fourth
  [coll]
  (get coll 3 nil))


(defn place-seg
  [[x1 y1 x2 y2] graph]
  (cond
    (= x1 x2)
    (reduce (fn [acc val]
              (let [coord (+ val (min y1 y2))]
                (update acc [x1 coord] (fn [prev]
                                           (if prev
                                             (inc prev)
                                             1)))))
            graph (range (inc (- (max y1 y2) (min y1 y2)))))
    (= y1 y2)
    (reduce (fn [acc val]
              (let [coord (+ val (min x1 x2))]
                (update acc [coord y1] (fn [prev]
                                         (if prev
                                           (inc prev)
                                           1)))))
            graph (range (inc (- (max x1 x2) (min x1 x2)))))
    :else
    (reduce (fn [acc val]
              (let [coord-x ((if (< x1 x2) + -) x1 val)
                    coord-y ((if (< y1 y2) + -) y1 val)]
                (update acc [coord-x coord-y] (fn [prev]
                                         (if prev
                                           (inc prev)
                                           1)))))
            graph (range (inc (- (max x1 x2) (min x1 x2)))))
    ))

(defn solve
  [in]
  (let [raw       (utils/load-edn-input in)
        vert-horz (->> raw
                       (filter (fn [[x1 y1 x2 y2]]
                                 (or (= x1 x2)
                                     (= y1 y2)))))
        graph     (loop [seg   (first raw)
                         rem   (rest raw)
                         graph (hash-map)]
                    (if seg
                      (recur (first rem) (rest rem) (place-seg seg graph))
                      graph)
                    )]
    (->> graph
         vals
         (filter #(>= % 2))
         count))
  )



(comment
  (solve "day5.edn")
  (solve-v2 "day1.edn"))
