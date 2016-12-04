(ns advent-of-code.day-four
  (:require [clojure.string :as str]))

(def test-input "aaaaa-bbb-z-y-x-123[abxyz]")

(def read-tokens
  (map #(rest (re-find #"([a-z-]+)(\d+)\[([a-z]+)\]" %))))

(def strip-hyphens-parse-id
  (map (fn [[name id checksum]]
         [(str/replace name #"-" "") (Integer/parseInt id) checksum])))

(defn checksum [name]
  (->> name
       frequencies
       (sort-by (juxt (fn [[k v]] (- v))
                      (fn [[k v]] k)))
       (take 5)
       (map first)
       str/join))

(def compute-checksum
  (map (fn [[name id candidate-checksum]]
         [name id candidate-checksum (checksum name)])))

(def valid-checksum?
  (filter (fn [[name id candidate-checksum our-checksum]]
            (= candidate-checksum our-checksum))))

(defn solve-one [input]
  (let [lines (str/split-lines input)
        valid-rooms (into []
                          (comp read-tokens strip-hyphens-parse-id compute-checksum valid-checksum?)
                          lines)]
    (reduce (fn [acc [_ num _ _]] (+ acc num)) 0 valid-rooms)))

(defn decrypt [offset letter]
  (let [small-offset (mod offset 26)
        key (drop-while #(not= letter %) (cycle "abcdefghijklmnopqrstuvwxyz"))]
    (nth key small-offset)))

(def decrypt-name (map (fn [[name id _ _]]
                         [(str/join (map (partial decrypt id) name)) id])))

(defn solve-two [input]
  (let [lines (str/split-lines input)
        valid-rooms (into []
                          (comp read-tokens
                                strip-hyphens-parse-id
                                compute-checksum
                                valid-checksum?
                                decrypt-name)
                          lines)]
    (clojure.pprint/pprint valid-rooms)))
