(ns advent-of-code.day-twenty-one
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def swap-position #"swap position (\d+) with position (\d+)")

(def swap-letter #"swap letter ([a-z]) with letter ([a-z])")

(def rotate #"rotate (left|right) (\d+) step|steps")

(def rotate-on-position #"rotate based on position of letter ([a-z])")

(def reverse-positions #"reverse positions (\d+) through (\d+)")

(def move-position #"move position (\d+) to position (\d+)")

(def test-input ["swap position 4 with position 0"
                 "swap letter d with letter b"
                 "reverse positions 0 through 4"
                 "rotate left 1 step"
                 "move position 1 to position 4"
                 "move position 3 to position 0"
                 "rotate based on position of letter b"
                 "rotate based on position of letter d"])

(defn rotate-left [s i]
  (let [idx (mod i (count s))]
    (str/join (concat (drop idx s)
                      (take idx s)))))

(defn rotate-right [s i]
  (let [idx (mod i (count s))
        reversed (reverse s)]
    (str/join (reverse (concat (drop idx reversed)
                               (take idx reversed))))))

(defmulti parse (fn [s word]
                  (cond (re-find swap-position s) :swap-position
                        (re-find swap-letter s) :swap-letter
                        (re-find rotate s) :rotate
                        (re-find rotate-on-position s) :rotate-on-position
                        (re-find reverse-positions s) :reverse-positions
                        (re-find move-position s) :move-position)))

(defmethod parse :swap-position [s word]
  (let [[_ pos-one pos-two] (re-find swap-position s)
        pos-one (Integer/parseInt pos-one)
        pos-two (Integer/parseInt pos-two)
        word (vec (seq word))]
    (str/join (assoc word pos-one (get word pos-two)
                     pos-two (get word pos-one)))))

(defmethod parse :swap-letter [s word]
  (let [[_ letter-one letter-two] (re-find swap-letter s)]
    (-> word
        (str/replace letter-one "-")
        (str/replace letter-two letter-one)
        (str/replace "-" letter-two))))

(defmethod parse :rotate [s word]
  (let [[_ direction steps] (re-find rotate s)
        direction (keyword direction)
        steps (Integer/parseInt steps)]
    (if (= :left direction) (rotate-left word steps)
        (rotate-right word steps))))

(defmethod parse :rotate-on-position [s word]
  (let [[_ letter] (re-find rotate-on-position s)
        index (str/index-of word letter)
        rotations (cond (< index 4) (inc index)
                        :else (+ 2 index))]
    (rotate-right word rotations)))

(defmethod parse :reverse-positions [s word]
  (let [[_ pos-one pos-two] (re-find reverse-positions s)
        pos-one (Integer/parseInt pos-one)
        pos-two (Integer/parseInt pos-two)]
    (str/join (concat (subvec (vec word) 0 pos-one)
                      (reverse (subvec (vec word) pos-one (inc pos-two)))
                      (subvec (vec word) (inc pos-two))))))

(defmethod parse :move-position [s word]
  (let [[_ pos-one pos-two] (re-find move-position s)
        pos-one (Integer/parseInt pos-one)
        pos-two (Integer/parseInt pos-two)
        indexed (vec (map-indexed (fn [x y] [x y]) word))
        char-one (get indexed pos-one)
        filtered (filter (fn [[i x]] (not= i pos-one)) indexed)
        de-indexed (map (fn [[i x]] x) filtered)]
    (str/join (concat (take pos-two de-indexed)
                      [(last char-one)]
                      (drop pos-two de-indexed)))))

(defn solve-one [word]
  (let [input (-> "day_21.txt"
                  io/resource
                  io/file
                  slurp
                  str/split-lines)]
    (reduce (fn [acc x]
              (println acc x "->" (parse x acc))
              (parse x acc)) word input)))

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn scramble [word input]
  (reduce (fn [acc x]
            (parse x acc)) word input))

(defn solve-two [word]
  (let [input (-> "day_21.txt"
                  io/resource
                  io/file
                  slurp
                  str/split-lines)
        perms (permutations word)]
    (reduce (fn [acc x]
              (when (zero? (mod acc 10000)) (println acc))
              (let [answer (scramble (str/join x) input)]
                (if (= answer word) (reduced (str/join x))
                    (inc acc)))) 0 perms)))
