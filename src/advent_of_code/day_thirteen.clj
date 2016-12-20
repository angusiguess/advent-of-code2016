(ns advent-of-code.day-thirteen
  (:require [clojure.string :as str]))

(def is-wall? (memoize (fn [fav [x y]]
                         (let [num (+
                                    fav
                                    (* x x)
                                    (* 3 x)
                                    (* 2 x y)
                                    y
                                    (* y y))
                               binary-string (Integer/toBinaryString num)]
                           (odd? (->> binary-string
                                      (filter #(= \1 %))
                                      count))))))

(defn is-open? [fav coord]
  (not (is-wall? fav coord)))

(defn neighbors [[x y]]
  (for [x-prime (range (dec x) (+ 2 x))
        y-prime (range (dec y) (+ 2 y))
        :when (and (nat-int? x-prime)
                   (nat-int? y-prime)
                   (not= x x-prime y y-prime)
                   (or (= x x-prime) (= y y-prime)))]
    [x-prime y-prime]))

(defn valid-neighbors [fav coord]
  (filter (partial is-open? fav) (neighbors coord)))

(defn valid-neighbors-unvisited [fav coord distances]
  (into [] (comp (filter (partial is-open? fav))
                 (filter  #(nil? (get distances %))))
        (neighbors coord)))

(defn print-maze [fav distances]
  (let [coords (for [y (range 100)
                     x (range 100)]
                 [x y])]
    (doseq [[x y] coords]
      (when (zero? x) (print "\n"))
      (cond (is-wall? fav [x y]) (print "#")
            (= [50 50] [x y]) (print "x")
            (get distances [x y]) (print (str (last (str (get distances [x y])))))
            :else (print ".")))))

(defn bfs [fav goal-node]
  (loop [q [[1 1]]
         distances {[1 1] 0}]
    (let [[first & rest] q]
      (if (= first goal-node) (do
                                (print-maze fav distances)
                                (get distances goal-node))
          (let [valid (valid-neighbors-unvisited fav first distances)]
            (recur
             (into (vec rest) valid)
             (reduce (fn [acc coord]
                       (if (get acc coord) acc
                           (assoc acc coord (inc (get distances first))))) distances valid)))))))

(defn bfs-two [fav]
  (loop [q [[1 1]]
         distances {[1 1] 0}]
    (let [[first & rest] q]
      (cond (>= (apply max (vals distances)) 51) (filter (fn [[k v]] (<= v 50)) distances)
            (empty? q) (do (print-maze fav distances)
                           distances)
            :else (let [valid (valid-neighbors-unvisited fav first distances)]
                    (recur
                     (into (vec rest) valid)
                     (reduce (fn [acc coord]
                               (if (get acc coord) acc
                                   (assoc acc coord (inc (get distances first))))) distances valid)))))))


(defn solve-first [fav]
  (search {[1 1] 0} [7 4] fav))

(defn solve-second [fav]
  (bfs-two fav))
