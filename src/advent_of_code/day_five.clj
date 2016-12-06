(ns advent-of-code.day-five
  (:require [clojure.string :as str]
            [clojure.core.async :as a])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

;; No libraries no masters
(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn key-sequence [key]
  (map (fn [n]
         (md5 (str/join [key (str n)])))))

(def five-leading-zeroes
  (map (fn [hash]
         (re-find #"^00000(.)" hash))))

(def five-leading-zeroes-part-two
  (map (fn [hash]
         (re-find #"^00000([0-7])(.).*" hash))))

(def password-character (filter (fn [match]
                                  (not (nil? match)))))


;; Okay why doesn't this work. I'm gonna just leak i/o
(defn solve-part-one [key]
  (let [password-chars (sequence (comp (key-sequence key)
                                       five-leading-zeroes
                                       password-character
                                       (take 8)) (range))]
    (doall password-chars)))

(defn solve-part-two [key]
  (let [password-chars (eduction (comp (key-sequence key)
                                       five-leading-zeroes-part-two
                                       password-character) (range))
        password (reduce (fn [acc [_ index char]]
                           (cond (= (:seen acc) #{"0" "1" "2" "3" "4" "5" "6" "7"}) (reduced acc)
                                 (get-in acc [:seen index]) (do (println acc) acc)
                                 :else (do
                                         (-> acc
                                             (update :seen conj index)
                                             (update :pw conj [index char])))))
                         {:seen #{}
                          :pw nil}
                         password-chars)]
    (->> password
         :pw
         (sort-by first)
         (map last))))
