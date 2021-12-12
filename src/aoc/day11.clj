(ns aoc.day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))


(def limit 10)


(defn process-step
  [squids]
  (->> squids
       (mapv (fn [line]
               (mapv inc line)))))

(defn flash
  "Return seq of index of squid that is going to flash"
  [adj squids]
  (->> adj
       (filterv (fn [coords]
                  (let [num (get-in squids coords)]
                   (> num 9))))
       set))

(defn valid-coord?
  [[x y]]
  (and (>= x 0)
       (>= y 0)
       (< x limit)
       (< y limit)))

(defn find-adjacent
  [flashed-set [x y]]
  (let [adj [[(inc x) y]
             [(inc x) (inc y)]
             [(inc x) (dec y)]
             [(dec x) y]
             [(dec x) (inc y)]
             [(dec x) (dec y)]
             [x (inc y)]
             [x (dec y)]]]
    (->> adj
         (filter valid-coord?)
         ;; Can only flash once this set don't care
         (remove flashed-set)))
  )

(defn update-adjacent
  [squids adjacent]
  (->> adjacent
       (reduce (fn [acc coords]
                 (update-in acc coords inc)) squids)))

(defn set-flashed-to-zero
  [squids flashed]
  (->> flashed
       (reduce (fn [acc coords]
                 (assoc-in acc coords 0)) squids))
  )

(defn format-input
  [line]
  (->> line
       str
       (mapv (comp edn/read-string str))))



(defn solve
  [in]
  (let [raw        [7612648217
                    7617237672
                    2853871836
                    7214367135
                    1533365614
                    6258172862
                    5377675583
                    5613268278
                    8381134465
                    3445428733]
        squids     (->> raw
                        (mapv format-input))
        all-coords (->> (mapcat (fn [x]
                                  (map (fn [y] [x y]) (range 0 limit))) (range 0 limit))
                        set)]
    (loop [step        0
           flash-count 0
           squids      squids]
      (if-not (= 100 step)
        (let [new-squid (process-step squids)
              [new-squid flash-set] (loop [cur-squid       new-squid
                                           total-flash-set #{}
                                           coords          all-coords]
                                      (let [just-flashed (flash coords cur-squid)
                                            ;after       (flash flash-adj updated-adj)
                                            ]
                                        (if (seq just-flashed)
                                          (let [flash-adj     (mapcat (partial find-adjacent (set/union just-flashed total-flash-set)) just-flashed)
                                                updated-adj   (update-adjacent cur-squid flash-adj)
                                                new-flash-set (set/union just-flashed total-flash-set)]
                                            (recur updated-adj new-flash-set (set/difference all-coords new-flash-set)))
                                          [(set-flashed-to-zero cur-squid total-flash-set) total-flash-set])))]
          (recur (inc step) (+ flash-count (count flash-set)) new-squid))
        flash-count))

    )
  )

(defn solve-v2
  [in]
  (let [raw        [7612648217
                    7617237672
                    2853871836
                    7214367135
                    1533365614
                    6258172862
                    5377675583
                    5613268278
                    8381134465
                    3445428733]
        squids     (->> raw
                        (mapv format-input))
        all-coords (->> (mapcat (fn [x]
                                  (map (fn [y] [x y]) (range 0 limit))) (range 0 limit))
                        set)]
    (loop [step 0
           squids      squids]
      (let [new-squid (process-step squids)
            [new-squid flash-set] (loop [cur-squid       new-squid
                                         total-flash-set #{}
                                         coords          all-coords]
                                    (let [just-flashed (flash coords cur-squid)]
                                      (if (seq just-flashed)
                                        (let [flash-adj     (mapcat (partial find-adjacent (set/union just-flashed total-flash-set)) just-flashed)
                                              updated-adj   (update-adjacent cur-squid flash-adj)
                                              new-flash-set (set/union just-flashed total-flash-set)]
                                          (recur updated-adj new-flash-set (set/difference all-coords new-flash-set)))
                                        [(set-flashed-to-zero cur-squid total-flash-set) total-flash-set])))]
        (if (= 100 (count flash-set))
          (inc step)
          (recur (inc step) new-squid))))))

(comment
  (solve "day11.txt")
  (solve-v2 "test8.edn")
  )
