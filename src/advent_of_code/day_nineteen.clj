(ns advent-of-code.day-nineteen)

;; Consider one elf
;; one gets all the presents

;; Consider two elves
;; one takes two, one gets all the presents

;; Consider three elves
;; one takes two, three takes one

;; Consider four elves
;; one takes two, three takes four
;; one takes three
;; 1 2 3 4
;; 1 3
;; 1

;; Consider five elves
;; one takes two, three takes four, five takes one
;; three takes five
;; 1 2 3 4 5
;; 3 5
;; 3

;; Six elves
;; one takes two, three takes four, five takes six
;; one takes three, five takes one

;; Seven elves
;; one takes two, three takes four, five takes six, seven takes one
;; three takes five, seven takes three

;; Eight elves
;; one takes two, three takes four, five takes six, seven takes eight
;; one takes three, five takes seven

(defn index [elves]
  (map-indexed (fn [i x] [i x]) elves))

(defn de-index [elves]
  (map (fn [[_ x]] x) elves))

(defn take-presents [elves]
  (cond (= 1 (count elves)) elves
        (odd? (count elves)) (->> elves
                                  index
                                  (filter (fn [[i x]] (even? i)))
                                  de-index
                                  (drop 1))
        (even? (count elves)) (->> elves
                                   index
                                   (filter (fn [[i x]] (even? i)))
                                   de-index)))

(defn solve-one [num-elves]
  (loop [elves (range 1 (inc num-elves))]
    (if (= 1 (count elves)) elves
        (recur (take-presents elves)))))

(defn rotate-seq [s]
  (concat (drop 1 s) (take 1 s)))

(defn slice-seq [s i]
  (concat (take i s) (drop (inc i) s)))

(defn solve-two-expensive [num-elves]
  (loop [elves (range 1 (inc num-elves))]
    (if (= 1 (count elves)) elves
        (recur (-> elves
                   (slice-seq (int (Math/floor (/ (count elves) 2))))
                   (rotate-seq))))))


;; Build the table of values
(doseq [i (range 1 101)] (println i (solve-two-expensive i)))

;; So given that we have two consecutive powers of three, we can figure out
;; where we are.
;; We know that at a power of 3 f(n) = n
;; After that we count up singles until the previous power of three
;; After which we count up doubles until the next power of three

;; Our puzzle input it 3014387, let's find the two powers of three between it

(defn powers-of-three [n]
  (lazy-seq (cons n (powers-of-three (* 3 n)))))

;; Need some cleanup for the remaining case

(defn solve-two-cheap [n]
  (let [power-of-three-below (last (take-while #(>= n %) (powers-of-three 1)))
        power-of-three-above (first (drop-while #(>= n %) (powers-of-three 1)))]
    (cond (= n power-of-three-below) power-of-three-below
          (= n power-of-three-above) power-of-three-above
          (< (- n power-of-three-below) power-of-three-below) (- n power-of-three-below)
          (> (- n power-of-three-below) power-of-three-below) (+ power-of-three-below (* 2 (- n
                                                                                              (* 2 power-of-three-below))))
          :else power-of-three-below)))

;; And let's verify that our closed form solution works

(= (flatten (map solve-two-expensive (range 1 101)))
   (map solve-two-cheap (range 1 101)))
