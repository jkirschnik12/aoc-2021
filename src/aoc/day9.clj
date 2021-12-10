(ns aoc.day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))

(def test-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")


(def floor-length (volatile! nil))
(def num-floors (volatile! nil))



(defn low-point?
  [floors floor-idx height-idx]
  (let [left           (when-not (zero? height-idx)
                         (get-in floors [floor-idx (dec height-idx)]))
        right          (when-not (= height-idx (dec @floor-length))
                         (get-in floors [floor-idx (inc height-idx)]))
        above          (when-not (zero? floor-idx)
                         (get-in floors [(dec floor-idx) height-idx]))
        below          (when-not (= floor-idx (dec @num-floors))
                         (get-in floors [(inc floor-idx) height-idx]))
        neighbors      (keep identity [left right above below])
        current-height (get-in floors [floor-idx height-idx])
        unique?        (->> neighbors
                            (filter #(= % current-height))
                            seq
                            not)
        ]
    (when (and unique?
               (= current-height (apply min (conj neighbors current-height)))
               current-height)
      current-height)))

(defn find-low-points-by-floor
  [floors floor-idx]
  (keep (fn [height-idx]
          (low-point? floors floor-idx height-idx)) (range @floor-length)))

(defn process-low-points
  [low-points]
  (->> low-points
       (map inc)
       (apply +)))

(defn solve
  [in]
  (let [raw    (utils/load-text-input in)
        floors (->> raw
                    (str/split-lines)
                    (mapv (fn [floor]
                            (->> floor
                                 (mapv #(edn/read-string (str %)))))))]
    (vreset! floor-length (count (first floors)))
    (vreset! num-floors (count floors))
    (loop [low-points []
           floor-idx  0]
      (if (get floors floor-idx)
        (let [low-points-for-floor (find-low-points-by-floor floors floor-idx)]
          (recur (concat low-points low-points-for-floor) (inc floor-idx)))
        (process-low-points low-points)))))

(comment
  (solve "day9.txt")
  (solve-v2 "test8.edn")
  )
