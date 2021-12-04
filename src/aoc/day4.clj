(ns aoc.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))

(def order [17, 2, 33, 86, 38, 41, 4, 34, 91, 61, 11, 81, 3, 59, 29, 71, 26, 44, 54, 89, 46, 9, 85, 62, 23, 76, 45, 24, 78, 14, 58, 48, 57, 40, 21, 49, 7, 99, 8, 56, 50, 19, 53, 55, 10, 94, 75, 68, 6, 83, 84, 88, 52, 80, 73, 74, 79, 36, 70, 28, 37, 0, 42, 98, 96, 92, 27, 90, 47, 20, 5, 77, 69, 93, 31, 30, 95, 25, 63, 65, 51, 72, 60, 16, 12, 64, 18, 13, 1, 35, 15, 66, 67, 43, 22, 87, 97, 32, 39, 82])

(defn format-board
  [board]
  (->> board
       (mapv str/trim)
       (mapv #(str/split % #"\s+"))
       (mapv (fn [line]
               (mapv (fn [num]
                       {:val (edn/read-string num)
                        :go  false}) line)))))

(defn horizontal-win?
  [board]
  (some (fn [line]
          (every? :go line)) board))

(defn vertical-win?
  [board]
  (let [transpose (apply mapv vector board)]
    (horizontal-win? transpose)))

(defn check-win-v2
  [boards]

  (->>
    (for [board (range (count boards))]
      (when (or (vertical-win? (nth boards board))
                (horizontal-win? (nth boards board)))
        board))
    (keep identity)
    vec))

(defn check-win
  [boards]
  (loop [cur    (first boards)
         rem    (rest boards)]
    (if cur
      (if (or (vertical-win? cur)
              (horizontal-win? cur))
        cur
        (recur (first rem) (rest rem))))))


(defn place-call
  [num-called boards]
  (let [bad (volatile! boards)]
    (doseq [board (range (count boards))]
      (doseq [lines (range 5)]
        (doseq [line (range 5)]
          (when (= num-called
                   (-> boards
                       (nth board)
                       (nth lines)
                       (nth line)
                       :val))
            (vswap! bad (fn [old]
                          (assoc-in old [board lines line :go] true)))))))
    @bad))

(defn sum-unmarked
  [board]
  (apply +
         (flatten
           (map (fn [lines]
                  (->> lines
                       (remove :go)
                       (map :val)
                       flatten)) board))))

(defn solve
  [in]
  (let [raw    (-> (utils/load-text-input in)
                   (str/split #"\n\n"))
        boards (->> raw
                    (mapv str/split-lines)
                    (mapv format-board))]
    (->> order
         (reduce (fn [acc val]
                   (let [out  (place-call val acc)
                         win? (check-win out)]
                     (if win?
                       (println (* val (sum-unmarked win?)))
                       out))) boards))))

(defn solve-v2
  [in]
  (let [raw    (-> (utils/load-text-input in)
                   (str/split #"\n\n"))
        boards (->> raw
                    (mapv str/split-lines)
                    (mapv format-board))]
    (->> order
         (reduce (fn [acc val]
                   (let [out  (place-call val acc)
                         win? (check-win-v2 out)]
                     (if (seq win?)
                       (do
                         (let [set-win (set win?)
                               new-out
                                       (vec (keep-indexed (fn [idx itm]
                                                            (when-not (set-win idx)
                                                              itm)) out))]
                           (if (empty? new-out)
                             (println (* val (sum-unmarked (first out))))
                             new-out)))
                       out))) boards))))
(comment
  (solve "day4.txt")
  (solve-v2 "day4.txt")
  )
