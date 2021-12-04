(ns aoc.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))

(defn calc-gamma
  [raw i]
  (->> raw
       (map #(subs % i (inc i)))
       (map edn/read-string)
       frequencies))

(defn construct-gamma
  [in]
  ((juxt (fn [in] (map #(if (> (get % 0) (get % 1))
                          0
                          1) in))
         (fn [in] (map #(if (< (get % 0) (get % 1))
                          0
                          1) in))) in))

(defn solve
  [in]
  (let [raw     (-> (utils/load-text-input in)
                    (str/split-lines))
        bin-len (count (first raw))]
    (loop [i         0
           cur-gamma []]
      (if (< i bin-len)
        (recur (inc i) (conj cur-gamma (calc-gamma raw i)))
        (map str/join (construct-gamma cur-gamma))))))


(defn solve-v2
  [cmpr in]
  (let [raw (-> (utils/load-text-input in)
                (str/split-lines))]
    (loop [i          0
           cur-bin []
           remaining  raw]
      (if (> (count remaining) 1)
        (do
          (let [freq    (calc-gamma remaining i)
                val     (if (cmpr (get freq 0) (get freq 1))
                          1
                          0)
                new-rem (filter #(= val
                                    (edn/read-string (subs % i (inc i))))
                                remaining)]


            (recur (inc i) (conj cur-bin val) new-rem)))
        (first remaining)))))


(comment
  (->> (solve "day3.txt")
       (map str/join)
       (map utils/bin-to-dec)
       (apply *))


  (->> ((juxt
          (partial solve-v2 <=)
          (partial solve-v2 >)) "day3.txt")
       (map str/join)
       (map utils/bin-to-dec)
       (apply *)))
