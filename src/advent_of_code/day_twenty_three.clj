(ns advent-of-code.day-twenty-three
  (:require [advent-of-code.assembunny :as asb]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn toggle [{:keys [opcode] :as instruction}]
  (case opcode
    :inc (assoc instruction :opcode :dec)
    (:dec :tgl) (assoc instruction :opcode :inc)
    :jnz (assoc instruction :opcode :cpy)
    :cpy (assoc instruction :opcode :jnz)))

(defmethod asb/exec-instruction :tgl [{:keys [pc] :as env} {:keys [arg1] :as instruction}]
  (let [arg1 (if (number? arg1) arg1 (get env arg1))]
    (cond-> env
        true (update :pc inc)
        (get-in env [:instructions (+ pc arg1)]) (update-in [:instructions (+ pc arg1)] toggle))))

(defn solve-one [filename]
  (let [instructions (->> filename
                          io/resource
                          io/file
                          slurp
                          str/split-lines
                          (map asb/read-instruction)
                          (map-indexed (fn [i x] [i x]))
                          (into {}))]
    (loop [env {:pc 0 :a 7 :b 0 :c 0 :d 0 :instructions instructions}]
      (if-let [next-instruction (get-in env [:instructions (:pc env)])]
        (do
          (recur (asb/exec-instruction env next-instruction)))
        env))))

(defn solve-two [filename]
  (let [instructions (->> filename
                          io/resource
                          io/file
                          slurp
                          str/split-lines
                          (map asb/read-instruction)
                          (map-indexed (fn [i x] [i x]))
                          (into {}))]
    (loop [env {:pc 0 :a 12 :b 0 :c 0 :d 0 :instructions instructions}]
      (if-let [next-instruction (get-in env [:instructions (:pc env)])]
        (do

          (recur (asb/exec-instruction env next-instruction)))
        env))))
