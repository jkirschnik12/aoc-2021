(ns aoc.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils]))






(def display
  {[:a :b :c :e :f :g] 0
   [:c :f] 1
   [:a :c :d :e :g] 2
   [:a :c :d :f :g] 3
   [:b :c :d :f] 4
   [:a :b :d :f :g] 5
   [:a :b :d :e :f :g] 6
   [:a :c :f] 7
   [:a :b :c :d :e :f :g] 8
   [:a :b :c :d :f :g] 9})




(defn solve
  [in]
  (let [raw             (utils/load-edn-input in)
        distinct-nums   #{1 4 7 8}
        distinct-counts (set (map (fn [v]
                                    (count (get display v))) distinct-nums))
        ]
    (println distinct-counts)
    (reduce (fn [acc [codes output]]
              (+ acc (reduce (fn [acc v]
                               (if (distinct-counts
                                     (count (str v)))
                                 (inc acc)
                                 acc)) 0 output)))
            0 raw)))

(def unique-in {2 1
                4 4
                3 7
                7 8})

(defn find-distinct
  [codes]
  (let [idx-map         (map-indexed (fn [idx val]
                                       {idx (count val)}) codes)
        distinct-counts (set
                          (keep
                            (fn [[k v]]
                              (when (= 1 v)
                                k))
                            (frequencies
                              (map-indexed #(get %2 %1) idx-map))))]
    (->> idx-map
         (keep-indexed
           (fn [idx my-map]
             (when (distinct-counts (get my-map idx))
               {(get unique-in (get my-map idx)) (first (keys my-map))})))
         (reduce merge)))
  )

(defn my-filter
  [codes match]
  (->> codes
       (filter #(= match (count %)))
       (first)
       (map #(keyword (str %)))
       set))

(defn my-filter-v2
  [codes match]
  (->> codes
       (filterv #(= match (count %)))
       (mapv (fn [in]
              (set (mapv #(keyword (str %)) in))))))

(defn find-a
  [codes]
  (let [val-1 (my-filter codes 2)
        val-7 (my-filter codes 3)]
    (first (set/difference val-7 val-1))))

(defn find-e-g
  [codes a c]
  (let [val-4   (my-filter codes 4)
        val-4-a (conj val-4 a)
        val-8   (my-filter codes 7)
        val-1   (my-filter codes 2)
        ;; has g
        len-3   (my-filter-v2 codes 5)
        _ (println len-3)
        blah (keep-indexed (fn [idx val]
                             [idx (set/intersection val-1 val)]) len-3)
        _ (println blah)
        idx-3 (first (keep (fn [[idx val]]
                             (when (= 2 (count val))
                               idx)) blah))
        two-or-fixe (keep (fn [[idx _val]]
                            (when (not= 2 (count val))
                              (get len-3 idx))) blah)
        two (first (filter #(% c) two-or-fixe))
        five (first (remove #(% c) two-or-fixe))
        stuff (set/difference two five)
        e (if (= (first stuff) c)
            (first stuff)
            (second stuff))
        val-3 (nth len-3 idx-3)
        _ (println val-3)

        val-e-g (set/difference val-8 val-4-a)
        g       (first (keep val-e-g val-3))
        e       (first (remove #(= g %) val-e-g))]
    [e g]))

;(defn find-b
;  [codes letters]
;  (let [not-b (into letters (my-filter codes 2))]
;    (-> (remove not-b codes)
;        first
;        str
;        keyword))
;  )


(defn find-d
  "ok"
  [codes letters]
  (let [val-1 (my-filter codes 2)
        len-5 (my-filter-v2 codes 5)
        diffs (keep-indexed (fn [idx val]
                              [idx (set/difference val val-1)]) len-5)
        idx-3 (first (keep (fn [[idx val]]
                       (when (= 3 (count val))
                         idx)) diffs))
        val-3 (nth len-5 idx-3)

        not-d (into letters val-1)]
    (-> (remove not-d val-3)
        first)))

(defn find-b
  "WRONG"
  [codes letters d]
  (let [val-1 (my-filter codes 2)
        len-6 (my-filter-v2 codes 6)
        num-0 (first (filter #(% d) len-6))
        _ (println num-0)

        not-b (into letters val-1)]
    (-> (remove not-b num-0)
        first)))

(defn find-c
  "ok"
  [codes]
  (let [val-1 (my-filter codes 2)
        len-6 (my-filter-v2 codes 6)
        blah (keep-indexed (fn [idx val]
                        [idx (set/intersection val-1 val)]) len-6)
        idx-6 (first (keep (fn [[idx val]]
                             (when (= 1 (count val))
                               idx)) blah))
        val-6 (nth len-6 idx-6)
        ]
    (first (set/difference val-1 val-6))
    ))



(defn find-f
  [codes c]
  (let [val-1 (my-filter codes 2)
        f     (first (remove #(= c %) val-1))]
    f))

()

(defn decode
  [[codes output]]
  (let [initial     {:a nil :b nil :c nil :d nil :e nil :f nil :g nil}
        nums        {0 nil 1 nil 2 nil 3 nil}
        counter     (volatile! -1)
        updated-out (reduce (fn [acc code]
                              (vswap! counter inc)
                              (if-let [len ((set (keys unique-in)) (count (str code)))]
                                (assoc acc @counter (get unique-in len))
                                acc)) nums output)
        codes-str   (map str codes)
        a           (find-a codes-str)
        c           (find-c codes-str)
        f           (find-f codes-str c)
        [e g] (find-e-g codes-str a c)
        d           (find-d codes-str #{a g})
        b           (find-b codes-str #{a e g} d)
        decrypt     (-> initial
                        (assoc a :a)
                        (assoc e :e)
                        (assoc g :g)
                        (assoc d :d)
                        (assoc b :b)
                        (assoc c :c)
                        (assoc f :f))
        output-str (map str output)]
    decrypt
    (vec (for [out-str output-str]
           (let [trans (mapv (fn [letter]
                              (let [lt-kw (keyword (str letter))]
                                (get decrypt lt-kw))) out-str)]
             (println (sort trans))
             (get display (vec (sort trans))))))



    ;updated-out
    ))



(defn solve-v2
  [in]
  (let [raw (utils/load-edn-input in)]
    (->> raw
         (mapv decode)
         ;;(apply +)
         )))





(comment
  ;;(solve "day8.edn")
  (solve-v2 "test8.edn")
  )
