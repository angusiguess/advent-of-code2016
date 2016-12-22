(ns advent-of-code.day-twenty-two
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data :as data]
            [clojure.data.priority-map :as pm]))

(def df-node #"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%")

(defn read-node [line]
  (let [[_ x y size used avail percent] (re-find df-node line)]
    {:x (Integer/parseInt x)
     :y (Integer/parseInt y)
     :size (Integer/parseInt size)
     :used (Integer/parseInt used)
     :avail (Integer/parseInt avail)
     :percent (Integer/parseInt percent)}))

(defn node-empty? [node]
  (zero? (:percent node)))

(defn node-not-empty? [node]
  (not (node-empty? node)))

(defn viable-pairs [nodes]
  (for [a nodes
        b nodes
        :when (and (not= a b)
                   (node-not-empty? a)
                   (<= (:used a) (:avail b)))]
    [a b]))



(defn solve-one []
  (let [nodes (->> "day_22.txt"
                   io/resource
                   io/file
                   slurp
                   str/split-lines
                   (drop 2)
                   (map read-node))]
    (count (viable-pairs nodes))))

(defn nodes []
  (let [nodes (->> "day_22.txt"
                   io/resource
                   io/file
                   slurp
                   str/split-lines
                   (drop 2)
                   (map read-node))]
    nodes))

(defn max-coords [nodes]
  [(apply max (map :x nodes))
   (apply max (map :y nodes))])

(def max-x 32)

(def max-y 29)

(defn goal-data [nodes]
  (first (filter (fn [{:keys [x y]}]
             (and (= x max-x)
                  (= y max-y))) nodes)))

(defn build-graph [nodes]
  (reduce (fn [acc {:keys [x y] :as node}]
            (assoc acc [x y] node)) {} nodes))

(defn has-capacity? [{:keys [avail]} capacity]
  (<= capacity avail))

(def adjacent (memoize (fn [[x y]]
                         (filter (fn [[x y]]
                                   (and (nat-int? x) (nat-int? y)
                                        (>= max-x x) (>= max-y y)))
                                 [[(inc x) y] [(dec x) y]
                                  [x (inc y)] [x (dec y)]]))))

(defn mv [graph n1 n2]
  (let [used (get-in graph [n1 :used])
        size (get-in graph [n1 :size])]
    (-> graph
        (assoc-in [n1 :used] 0)
        (assoc-in [n1 :avail] size)
        (update-in [n2 :used] + used)
        (update-in [n2 :avail] - used))))

(defn next-states [distance coord graph]
  (let [moves (into [] (comp (filter (fn [dest] (has-capacity? (get graph dest) (:used (get graph coord)))))
                             (map (fn [dest] [(inc distance) dest (mv graph coord dest)])))
                    (adjacent coord))]))

(defn viable-adjacent-pairs [nodes]
  (for [a nodes
        b nodes
        :when (and (not= a b)
                   (<= (:used a) (:avail b))
                   (get (into #{} (adjacent [(:x a) (:y a)])) [(:x b) (:y b)]))]
    [a b]))

(def adjacent-to? (memoize (fn adjacent-to? [[x y] [x1 y1]]
                             (get (into #{} (adjacent [x y])) [x1 y1]))))

(defn move-coord [coord a b]
  (if (and (= coord [(:x a) (:y a)])
           (adjacent-to? coord [(:x b) (:y b)]))
    [(:x b) (:y b)]
    coord))

(defn shortest-path [graph start dest]
  (loop [q [[0 [max-x 0] graph]]]
    (let [[first & rest] q
          [distance coord graph] first]
      (cond (nil? first) 100000
            (= coord dest) distance
            :else (recur (into (vec rest) (next-states distance coord graph)))))))

(defn enqueue [q moves dest]
  (reduce (fn [acc [dist coord g]]
            (assoc acc [dist coord g] (+ dist (shortest-path g coord dest))))
          q moves))

(defn a* [graph start dest]
  (loop [q (pm/priority-map [0 start graph] (shortest-path graph start dest))
         seen #{}]
    (let [[first & rest] (seq q)
          [[dist coord g :as tuple] pri] first]
      (println pri)
      (cond (nil? first) nil
            (= coord dest) dist
            :else (let [moves (map (fn [[a b]]
                                     [(inc dist)
                                      (move-coord coord a b)
                                      (mv g [(:x a) (:y a)]
                                          [(:x b) (:y b)])])
                                   (viable-adjacent-pairs (vals g)))]
                    (recur (-> q
                               (enqueue moves dest)
                               (dissoc tuple))
                           (into seen moves)))))))
