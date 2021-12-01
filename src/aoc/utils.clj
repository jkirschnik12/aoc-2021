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
