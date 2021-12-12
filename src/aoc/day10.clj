(ns aoc.day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))




(def open #{"[" "(" "{" "<"})
(def matches {"[" "]" "(" ")" "{" "}" "<" ">"})
(def points {")" 3 "]" 57 "}" 1197 ">" 25137})
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
    (if (and bracket
             (not error-char))
      (if (open bracket)
        (recur (first remaining) (rest remaining) (conj stack bracket) error-char)
        (recur (first remaining) (rest remaining) (pop stack) (pop-error-char bracket stack)))
      error-char)))


(defn calc-points
  [errors]
  (reduce (fn [acc error]
            (+ acc (get points error))) 0 errors))

(defn solve
  [in]
  (let [raw (str/split-lines (utils/load-text-input in))
        brackets (mapv (fn [input]
                         (->> input
                              (mapv str))) raw)]
    (->> brackets
         (keep process-line)
         calc-points)))

(comment
  (solve "day10.txt")
  (solve-v2 "test8.edn")
  )
