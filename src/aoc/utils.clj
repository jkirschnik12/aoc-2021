(ns aoc.utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(import 'java.io.PushbackReader)

(defn load-edn-input
  [resource-file]
  (edn/read (PushbackReader. (io/reader (io/resource resource-file)))))

(defn load-text-input
  [file]
  (slurp (io/resource file)))

(defn bin-to-dec
  [bin-str]
  (let [msb (dec (count bin-str))]
    (loop [i 0
           val 0]
      (if-not (> i msb)
        (if-not (zero? (edn/read-string (subs bin-str i (inc i))))
          (recur (inc i) (+ (Math/pow 2 (- msb i)) val))
          (recur (inc i) val))
          val))))
