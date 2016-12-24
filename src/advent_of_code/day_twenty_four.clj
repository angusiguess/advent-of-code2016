(ns advent-of-code.day-twenty-four
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn read-maze []
  (->> "day_24.txt"
       io/resource
       io/file
       slurp
       str/split-lines))

(defn build-maze [input]
  (into {} (comp (map-indexed (fn [i x] [i x]))
                 (mapcat (fn [[y row]] (map-indexed (fn [x col]
                                                      [[x y] col]) row))))
        input))

(defn get-points-of-interest [maze]
  (filter (fn [[k v]]
            (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} v)) maze))

(defn adjacent [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn traversable? [maze coord]
  (when-let [cell (get maze coord)]
    (not= cell \#)))

(def search (memoize (fn [maze source dest]
                       (loop [q [[0 source [source]]]
                              seen #{}]
                         (let [[first & rest] q
                               [dist coord path] first
                               next-moves (into [] (comp (filter (partial traversable? maze))
                                                         (filter (fn [x] (nil? (get seen x))))
                                                         (map (fn [x] [(inc dist) x (conj path x)])))
                                                (adjacent coord))]
                           (cond (nil? first) nil
                                 (= coord dest) first
                                 :else (recur (into (vec rest) next-moves)
                                              (into seen (map second next-moves)))))))))

(defn node-overlap [maze path]
  (set/intersection (into #{} (keys (into {} (get-points-of-interest maze))))
                    (into #{} path)))

;; There are 8 choose 2 = 28 possible path combinations, fewer if some paths
;; cover multiple points of interest.

(defn all-two-combinations [set]
  (let [indexed (map-indexed (fn [i x] [i x]) set)]
    (for [elem-a indexed
          elem-b indexed
          :when (and (not= elem-a elem-b)
                     (< (first elem-a)
                        (first elem-b)))]
      #{(last elem-a) (last elem-b)})))

(defn get-poi-combinations [maze]
  (-> maze
      get-points-of-interest
      keys
      all-two-combinations))

(defn solve-one [maze]
  (let [paths (into {}
                    (comp (map (fn [pair] (let [[first second] (seq pair)]
                                            [#{first second} (clojure.core/first (search maze first second))]))))
                    (get-poi-combinations maze))
        starting-point (first (first (filter (fn [[k v]] (= v \0)) (get-points-of-interest maze))))]
    (loop [visited #{starting-point}
           coord starting-point
           dist 0]
      (let [viable-paths (into [] (filter (fn [[coords dist]] (and (get coords coord)
                                                                   (empty? (set/intersection visited (disj coords coord))))))
                               (sort-by (fn [[k v]] v) paths))
            best-path (first viable-paths)]
        (if (empty? viable-paths) [visited coord dist]
            (recur (into visited (first best-path))
                   (first (disj (first best-path) coord))
                   (+ dist (last best-path))))))))

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn make-cycle [list elem])

(defn solve-two [maze]
  (let [poi (get-points-of-interest maze)
        first-coord (first (first (filter (fn [[k v]] (= v \0)) (get-points-of-interest maze))))]
    (reduce (fn [acc permutation]
              (let [pairs (partition 2 1 (concat [first-coord] permutation [first-coord]))
                    path-cost (+ (reduce + (map (fn [[a b]]
                                                  (first (search maze a b))) pairs)))]
                (if (< path-cost acc) path-cost acc))) Integer/MAX_VALUE
            (permutations (filter (fn [x] (not= x first-coord)) (keys poi))))))
