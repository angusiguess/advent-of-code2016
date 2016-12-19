(ns advent-of-code.day-seventeen
  (:import [java.security MessageDigest]
           [java.math BigInteger])
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn is-open? [c]
  (get #{\b \c \d \e \f} c))

(defn unlocked [s]
  (let [[up down left right] (take 4 s)]
    (cond-> []
      (is-open? up) (conj :U)
      (is-open? down) (conj :D)
      (is-open? left) (conj :L)
      (is-open? right) (conj :R))))

(defn valid-moves [[x y]]
  (cond-> []
    (> y 0) (conj :U)
    (< y 3) (conj :D)
    (> x 0) (conj :L)
    (< x 3) (conj :R)))

(defn coords-from-valid [[x y] valid]
  (map (fn [dir]
         (case dir
           :U [x (dec y)]
           :D [x (inc y)]
           :L [(dec x) y]
           :R [(inc x) y])) valid))

(defn coords-from-dir [[x y] direction]
  (case direction
    :U [x (dec y)]
    :D [x (inc y)]
    :L [(dec x) y]
    :R [(inc x) y]))

(defn hash-with-dirs [s dirs]
  (let [dir-string (->> dirs
                        (map name)
                        str/join)]
    (md5 (str/join [s dir-string]))))

(defn valid-and-unlocked [s coord]
  (set/intersection (into #{} (unlocked s))
                    (into #{} (valid-moves coord))))

(defn to-enqueue [coord path dirs]
  (map (fn [dir] [(coords-from-dir coord dir) (conj path dir)]) dirs))

(defn bfs [seed]
  (loop [q [[[0 0] []]]]
    (let [[first & rest] q
          [coord path] first
          dirs (valid-and-unlocked (hash-with-dirs seed path) coord)
          to-enqueue (to-enqueue coord path dirs)]
      (cond (= [3 3] coord) path
            :else (recur (into (vec rest) to-enqueue))))))

(defn solve-one [seed]
  (str/join (map name (bfs seed))))

(defn bfs-longer [seed]
  (loop [q [[[0 0] []]]
         complete-paths []]
    (let [[first & rest] q
          [coord path] first
          dirs (try (valid-and-unlocked (hash-with-dirs seed path) coord)
                    (catch Exception e
                      nil))
          to-enqueue (to-enqueue coord path dirs)]
      (cond (= [3 3] coord) (recur rest path)
            (nil? first) (count complete-paths)
            :else (recur (into (vec rest) to-enqueue) complete-paths)))))
