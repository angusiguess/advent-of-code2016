(ns advent-of-code.day-fourteen
  (:require [clojure.string :as str])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))


(defn key-sequence [key]
  (map (fn [n]
         (md5 (str/join [key (str n)])))))

(defn stretch-key [key]
  (loop [i 0
         s key]
    (if (> 2016 i)
      (recur (inc i) (md5 s))
      s)))

(defn stretched-key-sequence [key]
  (map (fn [n]
         (let [s (str/join [key (str n)])]
           (stretch-key (md5 s))))))



(def find-three-repeats
  (map (fn [[i x]]
         [i (->> x
                 (partition-by identity)
                 (filter (fn [x] (<= 3 (count x))))
                 first)])))

(def find-five-repeats
  (map (fn [[i x]]
         [i (->> x
                 (partition-by identity)
                 (filter (fn [x] (<= 5 (count x))))
                 first)])))

(def filter-repeats (filter (fn [[i x]] (seq x))))

(def print-progress (map (fn [[i x]] (when (zero? (mod i 1000))
                                       (println i))
                           [i x])))

(defn five-in-range [fives index char]
  (let [poss-keys (range (inc index) (+ index 1001))
        fives-in-range (select-keys fives poss-keys)]
    (boolean (seq (filter (fn [[k v]] (= (first v) char)) fives-in-range)))))

(defn solve-one [key]
  (let [five-seq (into {} (eduction (comp (key-sequence key)
                                          (map-indexed (fn [x y] [x y]))
                                          find-five-repeats
                                          filter-repeats
                                          (take 20)) (range)))
        three-seq (eduction (comp (key-sequence key)
                                  (map-indexed (fn [x y] [x y]))
                                  find-three-repeats
                                  filter-repeats) (range))]
    (reduce (fn [acc [i x]]
              (cond (= 64 (count acc)) (reduced acc)
                    (five-in-range five-seq i (first x)) (conj acc i)
                    :else acc))
            []
            three-seq)))

(defn solve-two [key]
  (let [five-seq (into {} (eduction (comp (stretched-key-sequence key)
                                          (map-indexed (fn [x y] [x y]))
                                          find-five-repeats
                                          filter-repeats
                                          (take 30)) (range)))
        three-seq (eduction (comp (stretched-key-sequence key)
                                  (map-indexed (fn [x y] [x y]))
                                  find-three-repeats
                                  filter-repeats) (range))]
    (println five-seq)
    (reduce (fn [acc [i x]]
              (cond (= 64 (count acc)) (reduced acc)
                    (five-in-range five-seq i (first x)) (do (conj acc i))
                    :else acc))
            []
            three-seq)))
