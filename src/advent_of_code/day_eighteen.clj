(ns advent-of-code.day-eighteen
  (:require [clojure.string :as str]))

(def input ".^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^")

(defn trap? [c]
  (= \. c))

(defn safe? [c]
  (not (trap? c)))

(defn next-state [left center right]
  (if (or (and (trap? left) (trap? center) (safe? right))
          (and (safe? left) (trap? center) (trap? right))
          (and (trap? left) (safe? center) (safe? right))
          (and (safe? left) (safe? center) (trap? right))) \^
      \.))

(defn next-row [row]
  (let [padded (concat [\.] row [\.])]
    (map #(apply next-state %) (partition 3 1 padded))))

(defn gen-field [input reps]
  (loop [i 0
         acc [(seq input)]]
    (if (> reps i)
      (recur (inc i) (conj acc (next-row (last acc))))
      acc)))

(defn solve-one [input reps]
  (let [field (gen-field input reps)]
    (->> field
         (map str/join)
         str/join
         (filter (fn [c] (= \. c)))
         count)))

;; Ugh, let's speed it up a little bit

(defn gen-rows [row]
  (lazy-seq (cons row (gen-rows (next-row row)))))

(defn solve-two [input reps]
  (reduce (fn [acc x]
            (+ acc
               (count (filter trap? x)))) 0 (take reps (gen-rows input))))
