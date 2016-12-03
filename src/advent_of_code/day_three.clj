(ns advent-of-code.day-three
  (:require [clojure.string :as str]))

(def test-input "  5 10 25  ")

(defn read-triangle [line]
  (rest (re-find #"\s+(\d+)\s+(\d+)\s+(\d+)" line)))

(def read-triangles (map read-triangle))

(def parse-integers (map (fn [triangle]
                           (map #(Integer/parseInt %) triangle))))

(def chunk-triangles (partition-all 3))

(def rearrange-triangles (mapcat (fn [[[a1 b1 c1]
                                       [a2 b2 c2]
                                       [a3 b3 c3]]]
                                   [[a1 a2 a3]
                                    [b1 b2 b3]
                                    [c1 c2 c3]])))


(def valid-triangles (filter (fn [[a b c]]
                              (and (> (+ a b) c)
                                   (> (+ b c) a)
                                   (> (+ c a) b)))))

(defn solve-one [input]
  (let [lines (str/split-lines input)
        valid-triangles (comp read-triangles
                              parse-integers
                              valid-triangles)]
    (count (into [] valid-triangles lines))))

(defn solve-two [input]
  (let [lines (str/split-lines input)
        valid-triangles (comp read-triangles
                              parse-integers
                              chunk-triangles
                              rearrange-triangles
                              valid-triangles)]
    (count (into [] valid-triangles lines))))
