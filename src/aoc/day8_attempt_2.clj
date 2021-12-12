(ns aoc.day8-attempt-2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))

(def display
  {#{:a :b :c :e :f :g}    0
   #{:c :f}                1
   #{:a :c :d :e :g}       2
   #{:a :c :d :f :g}       3
   #{:b :c :d :f}          4
   #{:a :b :d :f :g}       5
   #{:a :b :d :e :f :g}    6
   #{:a :c :f}             7
   #{:a :b :c :d :e :f :g} 8
   #{:a :b :c :d :f :g}    9})

(def reverse-display
  {0 #{:a :b :c :e :f :g}
   1 #{:c :f}
   2 #{:a :c :d :e :g}
   3 #{:a :c :d :f :g}
   4 #{:b :c :d :f}
   5 #{:a :b :d :f :g}
   6 #{:a :b :d :e :f :g}
   7 #{:a :c :f}
   8 #{:a :b :c :d :e :f :g}
   9 #{:a :b :c :d :f :g}})

(def unique-in {2 1
                4 4
                3 7
                7 8})

(defn find-distinct
  [codes]
  (->> codes
       (keep
         (fn [code]
           (when-let [cnt (#{7 4 3 2} (count code))]
             {(get unique-in cnt) (set code)})))
       (reduce merge)))

(defn find-non-distinct
  [distinct codes]
  (->> codes
       (keep #(when-not (get distinct %)
                %))
       vec))

(defn find-3
  [codes one]
  (->> codes
       (filterv #(= (count %) 5))
       (map (fn [in]
              [in (set/intersection one in)]))
       (filter #(= (count (second %)) 2))
       first
       first))

#_(defn find-9
    [codes three four]
    (let [essentially-9 (set/intersection three)]
      (->> codes
           (filterv #(= (count %) 6))
           (map (fn [in]
                  [in (set/intersection one in)]))
           (filter #(= (count (second %)) 2))
           first
           first))
    )

(defn find-2-5
  [codes nine eight three]
  (let [just-e   (set/difference eight nine)
        _        (println just-e)
        two-five (->> codes
                      (filterv #(= (count %) 5))
                      (remove (partial = three)))
        _        (println two-five)
        two      (->> two-five
                      (filter #(some just-e %))
                      first)
        five     (->> two-five
                      (remove #(some just-e %))
                      first)]
    [two five]))

(defn find-0-6
  [codes nine eight five]
  (let [just-e (set/difference eight nine)
        six    (set/union five just-e)
        _      (println six)
        zero   (->> codes
                    (filterv #(= (count %) 6))
                    (remove (partial = nine))
                    (remove (partial = six))
                    first)]
    [zero six]))

(defn decode
  [[codes output]]
  (let [codes        (map #(map (fn [in]
                                  (keyword (str in))) (str %)) codes)
        output        (map #(set (map (fn [in]
                                        (keyword (str in))) (str %))) output)
        distinct     (find-distinct codes)
        four         (get distinct 4)
        seven        (get distinct 7)
        one          (get distinct 1)
        eight        (get distinct 8)
        non-distinct (->> codes
                          (mapv vec)
                          (mapv set)
                          (find-non-distinct distinct))
        three        (find-3 non-distinct one)
        nine         (set/union three four)
        [two five] (find-2-5 non-distinct nine eight three)
        [zero six] (find-0-6 non-distinct nine eight five)
        translate {zero 0
                   one 1
                   two 2
                   three 3
                   four 4
                   five 5
                   six 6
                   seven 7
                   eight 8
                   nine 9}]
    (->> output
         (mapv #(get translate %))
         str/join
         Integer.)))


(defn solve-v2
  [in]
  (let [raw (utils/load-edn-input in)]
    (->> raw
         (mapv decode)
         (apply +)
         )))

(comment
  (solve-v2 "day8.edn")
  )
