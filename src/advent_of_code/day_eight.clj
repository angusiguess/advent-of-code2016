(ns advent-of-code.day-eight
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def display-height 6)

(def display-width 50)

(defmulti read-instruction (fn [env s] (re-find #"rect|rotate column|rotate row" s)))

(defn light-pixels [width height]
  (into #{} (for [x (range width)
                  y (range height)]
              [x y])))

(defmethod read-instruction "rect" [env string]
  (let [[_ width height] (re-find #"rect (\d+)x(\d+)" string)
        width (Integer/parseInt width)
        height (Integer/parseInt height)]
    (set/union (light-pixels width height) env)))

(defmethod read-instruction "rotate column" [env string]
  (let [[_ column offset] (re-find #"x=(\d+) by (\d+)" string)
        column (Integer/parseInt column)
        offset (Integer/parseInt offset)]
    (into #{} (map (fn [[x y]]
                     (if (= x column)
                       [x (mod (+ y offset) display-height)]
                       [x y])) env))))

(defmethod read-instruction "rotate row" [env string]
  (let [[_ row offset] (re-find #"y=(\d+) by (\d+)" string)
        row (Integer/parseInt row)
        offset (Integer/parseInt offset)]
    (into #{} (map (fn [[x y]]
                     (if (= y row)
                       [(mod (+ x offset) display-width) y]
                       [x y])) env))))

(defn solve-one [input]
  (let [lines (str/split-lines input)]
    (reduce read-instruction #{} lines)))

(defn solve-two [input]
  (let [lines (str/split-lines input)
        pixels (reduce read-instruction #{} lines)
        screen (for [y (range display-height)
                     x (range display-width)]
                 [x y])]
    (doseq [[x y] screen]
      (when (zero? x) (print "\n"))
      (if (get pixels [x y]) (print "x")
          (print " ")))))
