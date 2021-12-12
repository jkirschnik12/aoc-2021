(ns aoc.day9-2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))

(def test-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")


(def floor-length (volatile! nil))
(def num-floors (volatile! nil))


(defn get-edges
  [floors floor-idx height-idx]
  (let [left            (when-not (zero? height-idx)
                          (when (some-> (get-in floors [floor-idx (dec height-idx)])
                                        (not= 9))
                            [floor-idx (dec height-idx)]))
        right           (when-not (= height-idx (dec @floor-length))
                          (when (some-> (get-in floors [floor-idx (inc height-idx)])
                                        (not= 9))
                            [floor-idx (inc height-idx)]))
        above           (when-not (zero? floor-idx)
                          (when (some-> (get-in floors [(dec floor-idx) height-idx])
                                        (not= 9))
                            [(dec floor-idx) height-idx]))
        below           (when-not (= floor-idx (dec @num-floors))
                          (when (some-> (get-in floors [(inc floor-idx) height-idx])
                                        (not= 9))
                            [(inc floor-idx) height-idx]))
        basin-neighbors (keep identity [left right above below])]
    (set basin-neighbors)))


(defn find-basin
  [floors floor-idx low-point-idx]
  (loop [heights-in-basin #{[floor-idx low-point-idx]}
         basin-internal   #{}]
    (if (empty? heights-in-basin)
      basin-internal
      (let [[current-floor-idx height-idx] (first heights-in-basin)
            new-basin (set/difference (get-edges floors current-floor-idx height-idx) basin-internal)]
        (recur (set/union new-basin (disj heights-in-basin [current-floor-idx height-idx]))
               (conj basin-internal [current-floor-idx height-idx])))))

  )

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
                            not)]
    (when (and unique?
               (= current-height (apply min (conj neighbors current-height)))
               current-height)
      (find-basin floors floor-idx height-idx))))

(defn find-low-points-by-floor
  [floors floor-idx]
  (keep (fn [height-idx]
          (low-point? floors floor-idx height-idx)) (range @floor-length)))

(defn process-low-points
  [low-points]
  (->> low-points
       (map inc)
       (apply +)))

(defn process-basins
  [basins]
  (->> basins
       (map count)
       sort
       (take-last 3)
       (apply *)))

(defn solve
  [in]
  (let [
        raw    (utils/load-text-input in)
        ;;raw    test-input
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
        (process-basins low-points)))))

(comment
  (solve "day9.txt")
  (solve-v2 "test8.edn")
  )
