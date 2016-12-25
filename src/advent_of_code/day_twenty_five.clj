(ns advent-of-code.day-twenty-five
  (:require [advent-of-code.assembunny :as asb]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn solve-one []
  (let [instructions (->> "day_25.txt"
                   io/resource
                   slurp
                   str/split-lines
                   (map asb/read-instruction))]
    (loop [env {:pc 0 :a 180 :b 0 :c 0 :d 0 :out []}]
      (if-let [next-instruction (first (drop (:pc env) instructions))]
        (if (= 29 (:pc env)) env
          (recur (asb/exec-instruction env next-instruction)))
        env))))

;; Read environment output, breakpoint on end of program for repetition point
;; figured out the number is binary backwards, number in d was 2551, 2730 is our
;; desired number, add 179 voila
