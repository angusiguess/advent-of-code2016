(ns advent-of-code.day-seven
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn is-abba? [str]
  (and (= 4 (count str))
       (= 2 (count (into #{} str)))
       (= str (reverse str))))

(defn in-brackets [str] (map last (re-seq #"\[([^\]]*)\]" str)))

(defn not-in-brackets [str] (mapcat #(remove nil? (rest %))
                                    (re-seq #"(^[^\[]*)\[|\]([^\]]*)$|\]([^\[]*)\[" str)))

(defn is-aba? [[one two three]]
    (and (= one three)
         (not= one two)))

(defn corresponding-babs [abas]
  (into #{} (map (fn [[one two three]]
                   (seq [two one two])) abas)))

(defn is-tls? [str]
  (and
   (not (some is-abba? (mapcat #(partition 4 1 %) (in-brackets str))))
   (some is-abba? (mapcat #(partition 4 1 %) (not-in-brackets str)))))

(defn is-ssl? [str]
  (let [abas (into #{} (filter is-aba? (mapcat (partial partition 3 1) (not-in-brackets str))))
        babs (into #{} (filter is-aba? (mapcat (partial partition 3 1) (in-brackets str))))]
    (boolean (seq (set/intersection (corresponding-babs abas) babs)))))

(defn solve-one [input]
  (let [lines (str/split-lines input)]
    (count (filter is-tls? lines))))

(defn solve-two [input]
  (let [lines (str/split-lines input)]
    (count (filter is-ssl? lines))))
