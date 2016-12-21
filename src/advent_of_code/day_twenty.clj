(ns advent-of-code.day-twenty
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-interval [s]
  (let [[_ begin end] (re-find #"(\d+)-(\d+)" s)]
    [(Long/parseLong begin) (Long/parseLong end)]))

(defn read-intervals [input]
  (->> input
       str/split-lines
       (map read-interval)
       (sort-by (juxt first last))))

;; starting interval is [0 0]

;; if the next entry begin is <= current end

(defn solve-one [input]
  (let [intervals (read-intervals input)]
    (reduce (fn [acc [start end]]
              (let [[s e] acc]
                (println s e start end)
                (if (>= (inc e) start) [s end]
                    (reduced (inc e))))) [0 0] intervals)))

;; We define a merge of two intervals i1 and i2
;; Where [a1 b1] [a2 b2]
;; When b1 is before a2, there's no merge
;; Otherwise sort them and take the ends

(defn can-merge? [[a1 b1] [a2 b2]]
  (>= (inc b1) a2))

(defn merge [[a1 b1] [a2 b2]]
  (let [sorted (sort [a1 b1 a2 b2])]
    [(first sorted) (last sorted)]))

(defn merge-intervals [intervals]
  (reduce (fn [acc [a2 b2]]
            (let [[a1 b1] (last acc)]
              (if (can-merge? [a1 b1] [a2 b2]) (conj (vec (butlast acc)) (merge [a1 b1] [a2 b2]))
                  (conj acc [a2 b2])))) [[0 0]] intervals))

(defn non-overlap [[_ b1] [a2 _]]
  (- a2 b1 1))

(defn solve-two [input]
  (let [intervals (read-intervals input)
        merged (merge-intervals intervals)
        pairs (partition 2 1 merged)]
    (reduce + (map #(apply non-overlap %) pairs))))

(defn examine []
  (take 4 (read-intervals (slurp (io/file (io/resource "day20.txt"))))))
