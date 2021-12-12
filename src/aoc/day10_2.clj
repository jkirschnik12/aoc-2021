(ns aoc.day10-2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))




(def open #{"[" "(" "{" "<"})
(def matches {"[" "]" "(" ")" "{" "}" "<" ">"})
(def points {")" 1 "]" 2 "}" 3 ">" 4})
;(def close #{"]" ")" "}" ">"})


(defn pop-error-char
  [bracket stack]
  (let [match (last stack)
        expected (get matches match)]
    (when-not (= bracket expected)
      bracket))
  )

(defn process-line
  [line]
  (loop [bracket (first line)
         remaining (rest line)
         stack []
         error-char nil]
    (if bracket
      (if (open bracket)
        (recur (first remaining) (rest remaining) (conj stack bracket) error-char)
        (recur (first remaining) (rest remaining) (pop stack) (or error-char
                                                                  (pop-error-char bracket stack))))
      (when-not error-char
        (map #(get matches %) stack)))))


(defn calc-points
  [lines]
  (reduce (fn [acc line]
            (-> acc
                (* 5)
                (+ (get points line)))) 0 lines))

(defn get-middle
  [sorted-coll]
  (let [size (count sorted-coll)
        middle-index (quot size 2)]
    (nth sorted-coll middle-index)))

(defn solve
  [in]
  (let [raw (str/split-lines (utils/load-text-input in))
        brackets (mapv (fn [input]
                         (->> input
                              (mapv str))) raw)]
    (->> brackets
         (keep process-line)
         (map reverse)
         (map calc-points)
         sort
         get-middle)))

(comment
  (solve "day10.txt")
  (solve-v2 "test8.edn")
  )
