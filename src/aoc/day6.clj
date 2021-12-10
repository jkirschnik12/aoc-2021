(ns aoc.day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))






(defn process-tick
  [fish]
  (if (= 0 fish)
    [6 8]
    [(dec fish)]))

(defn solve
  [in]
  (let [raw (utils/load-edn-input in)]
    (count (reduce (fn [acc i]
               (mapcat (fn [fish]
                         (process-tick fish)) acc)) raw (range 0 80))))
  )

(defn process-tick-v2
  [fish]
  (println fish)
  (let [zeroes (get fish 0)
        out   (->> fish
                   (reduce-kv (fn [m k v]
                                (println [k v])
                                (if (= k 0)
                                  m
                                  (assoc m (dec k) v))) {}))]
    (-> out
        (update 6 #(+ zeroes %))
        (assoc 8 zeroes)))
  )

(defn solve-v2
  [in]
  (let [raw (utils/load-edn-input in)
        starting-map {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0}
        freq-map (merge starting-map (frequencies raw))]
    (println freq-map)
    (->> (reduce (fn [acc i]
                   (process-tick-v2 acc)) freq-map (range 0 256))
         vals
         (apply +)))
  )

(comment
  (solve "day6.edn")
  (solve-v2 "day6.edn")
  )
