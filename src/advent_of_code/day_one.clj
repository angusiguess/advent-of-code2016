(ns advent-of-code.day-one
  (:require [clojure.string :as str]))

(def day-one-input "L5, R1, R3, L4, R3, R1, L3, L2, R3, L5, L1, L2, R5, L1, R5, R1, L4, R1, R3, L4, L1, R2, R5, R3, R1, R1, L1, R1, L1, L2, L1, R2, L5, L188, L4, R1, R4, L3, R47, R1, L1, R77, R5, L2, R1, L2, R4, L5, L1, R3, R187, L4, L3, L3, R2, L3, L5, L4, L4, R1, R5, L4, L3, L3, L3, L2, L5, R1, L2, R5, L3, L4, R4, L5, R3, R4, L2, L1, L4, R1, L3, R1, R3, L2, R1, R4, R5, L3, R5, R3, L3, R4, L2, L5, L1, L1, R3, R1, L4, R3, R3, L2, R5, R4, R1, R3, L4, R3, R3, L2, L4, L5, R1, L4, L5, R4, L2, L1, L3, L3, L5, R3, L4, L3, R5, R4, R2, L4, R2, R3, L3, R4, L1, L3, R2, R1, R5, L4, L5, L5, R4, L5, L2, L4, R4, R4, R1, L3, L2, L4, R3")

(defn parse [token]
  (let [first (first token)
        rest (Integer/parseInt (str/join (rest token)))]
    [first rest]))

(defn tokenize []
  (let [split (str/split day-one-input #", ")]
    (map parse split)))

(defn turn [dir turn]
  (case dir
    :n (if (= turn \L) :w :e)
    :e (if (= turn \L) :n :s)
    :w (if (= turn \L) :s :n)
    :s (if (= turn \L) :e :w)))

(defn travel [dir {:keys [x y]} steps]
  (case dir :n (map (fn [y] [x y]) (range (inc y) (inc (+ y steps))))
        :e (map (fn [x] [x y]) (range (inc x) (inc (+ x steps))))
        :w (map (fn [x] [x y]) (reverse (range (- x steps) x)))
        :s (map (fn [y] [x y]) (reverse (range (- y steps) y)))))

(defn distance [directions]
  (reduce (fn [acc [l-or-r num]]
            (let [new-dir (turn (:dir acc) l-or-r)]
              (case new-dir
                :n (-> acc (assoc :dir new-dir)
                       (update :y + num)
                       (update :travel concat (travel new-dir acc num)))
                :e (-> acc (assoc :dir new-dir)
                       (update :x + num)
                       (update :travel concat (travel new-dir acc num)))
                :w (-> acc (assoc :dir new-dir)
                       (update :x - num)
                       (update :travel concat (travel new-dir acc num)))
                :s (-> acc (assoc :dir new-dir)
                       (update :y - num)
                       (update :travel concat (travel new-dir acc num))))))
          {:dir :n
           :x 0
           :y 0}
          directions))

(defn granular-distance [directions]
  (let [pairs (partition 2 1 directions)]
    (reduce (fn [acc [first second]]
              (let [x-one (:x first)
                    x-two (:x second)
                    y-one (:y first)
                    y-two (:y second)]
                (if (= x-one x-two) (conj acc (map (fn [y]
                                                     [x-one y]) (range y-one y-two)))))) pairs)))

(defn total-distance [{:keys [x y]}]
  (+ (Math/abs x) (Math/abs y)))

(defn first-visited-twice [locations]
  (reduce (fn [acc [x y]]
            (if (get acc [x y])
              (reduced (+ (Math/abs x) (Math/abs y)))
              (conj acc [x y]))) #{} locations))

;; Well that got dirty but

(first-visited-twice (:travel (distance (tokenize))))
