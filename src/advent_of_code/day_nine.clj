(ns advent-of-code.day-nine
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [clojure.java.io :as io]))

(defn strip-whitespace [s]
  (-> s
      (str/replace " " "")
      (str/replace "\n" "")))

(defn is-marker? [s]
  (boolean (and (string? s)
                (re-find #"^\((\d+)x(\d+)\)" s))))

(defn parse-marker [marker]
  (let [[found num-chars repetitions] (re-find #"(\d+)x(\d+)" marker)]
    (when found [(Integer/parseInt num-chars) (Integer/parseInt repetitions)])))

(defn chomp-marker [s]
  (str/join (rest (drop-while #(not= \) %) s))))

(defn parse-marker [marker]
  (let [[found num-chars repetitions] (re-find #"(\d+)x(\d+)" marker)]
    (when found [(Integer/parseInt num-chars) (Integer/parseInt repetitions)])))

(defn decompress-one [s]
  (loop [s s
         s-decomp ""]
    (cond (empty? s) s-decomp
          (is-marker? s) (let [[num-chars repetitions] (parse-marker s)
                               s-without-marker (chomp-marker s)
                               to-repeat (str/join (take num-chars s-without-marker))
                               repeated (str/join (seq (repeat repetitions to-repeat)))]
                           (recur (str/join (drop num-chars s-without-marker))
                                  (str/join [s-decomp repeated])))
          :else (recur (str/join (drop-while #(not= \( %) s))
                       (str/join [s-decomp (str/join (take-while #(not= \( %) s))])))))

(defn solve-one [s]
  (-> s
      strip-whitespace
      decompress-one
      count))

;; Part two

(defn chars->weights [tokens]
  (map (fn [x] (if (is-marker? x) x
                   1)) tokens))

(defn update-weights [tokens multiplier]
  (map (fn [x] (if (is-marker? x) x
                   (* multiplier x))) tokens))

(defn tokenize [s]
  (->> s
       (re-seq #"([A-Z]|\(\d+x\d+\))")
       (map last)))

(defn solve-two [s]
  (let [tokens (chars->weights (tokenize s))]
    (loop [[first & rest] tokens
           acc 0]
      (cond (nil? first) acc
            (number? first) (recur rest (+ acc first))
            :else (let [[n repetitions] (parse-marker first)]
                    (recur (concat (update-weights (take n rest) repetitions)
                                   (drop n rest)) acc))))))
