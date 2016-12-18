(ns advent-of-code.day-sixteen
  (:require [clojure.string :as str]))

(def input "01111010110010011")

(defn negate [b]
  (->> b
       (map #(if (= \0 %) 1 0))
       str/join))

(defn dragon-curve [input]
  (let [a input
        b (negate (str/reverse input))]
    (str/join "0" [a b])))

(defn fill-disk [input size]
  (loop [input input]
    (if (> (count input) size) (str/join (take size input))
        (recur (dragon-curve input)))))

(defn checksum [input]
  (let [pairs (partition 2 input)]
    (->> pairs
         (map (fn [[a b]]
                (if (= a b) \1 \0)))
         str/join)))

(defn iterate-checksum [input]
  (loop [chk (checksum input)]
    (if (even? (count chk)) (recur (checksum chk))
        chk)))

(defn solve-one [input size]
  (iterate-checksum (fill-disk input size)))
