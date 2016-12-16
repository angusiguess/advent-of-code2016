(ns advent-of-code.day-fifteen)

;; Okay, so basically I can never remember how modular arithmetic works
;; probably I never learned it properly.

;; But what I can say is that knowing the size of each disc, we should
;; be able to determine how big the cycle length is, which is the number
;; of states before our discs return to their original states

;; We do this with lcm

;; Disc #1 has 13 positions; at time=0, it is at position 1.
;; Disc #2 has 19 positions; at time=0, it is at position 10.
;; Disc #3 has 3 positions; at time=0, it is at position 2.
;; Disc #4 has 7 positions; at time=0, it is at position 1.
;; Disc #5 has 5 positions; at time=0, it is at position 3.
;; Disc #6 has 17 positions; at time=0, it is at position 5.

(defn abs [a] (Math/abs a))

(defn gcd [a b]
  (if (zero? b) a
      (recur b (mod a b))))

(defn lcm
  ([a b] (/ (abs (* a b))
            (gcd a b)))
  ([a b & rest]
   (lcm a (apply lcm b rest))))

;; So each disc returns to its initial position at

(def cycle-length (lcm 13 19 3 7 5 17))

(def cycle-length-two (lcm 13 19 3 7 5 17 11))


;; So that's our search space. I might do this with an embarrassing
;; filter. But first let's check the example problem

;; Disc #1 has 5 positions; at time=0, it is at position 4.
;; Disc #2 has 2 positions; at time=0, it is at position 1.

;; 0 1 2 3 4
;; 4 0 1 2 3

;; Ten positions is a small search space! We need disc #1 to be
;; in position 4 at time 0 and we need disc 2 to be in position
;; 0 at time 0
;; t mod 4 - 5 = 0
;; t mod 2 - 2 = 0

;; okay we're close but now my head is starting to hurt. Let's
;; try to figure out with one disc the time offset of any position.

;; Disc has 4 positions
;; 0 is any time t % 4 = 0
;; 1 is any time t % 4 = 1
;; and so on.

;; so for this one our position 0 is 4
;; and our desired position is 0, so we want 1

;; so we want t + 1 mod 4 = 1
;; and t + 2 mod 2 = 1

;; Okay, next day! I think we have what we need to figure out our modulos
;; Let's reexamine our values

;; Disc #1 has 13 positions; at time=0, it is at position 1.
;; Disc #2 has 19 positions; at time=0, it is at position 10.
;; Disc #3 has 3 positions; at time=0, it is at position 2.
;; Disc #4 has 7 positions; at time=0, it is at position 1.
;; Disc #5 has 5 positions; at time=0, it is at position 3.
;; Disc #6 has 17 positions; at time=0, it is at position 5.

;; we would want it to be at position (0 - 1 where 1 is the time offset)
;; but the disc also has an initial position. We can think of this as
;; relabelling the positions.

;; For disc one that means 0 1 2 3 4 5 6 7 8 9 10 11 12
;;                      -> 1 2 3 4 5 6 7 8 9 10 11 12 0
;; giving us a mapping that shows we actually want to be at
;; position 11. So we should be able to subract time and position
;; values

;; Okay, so here's the equation we need to satisfy
;; let t be time
;; (t + offset) % positions = (0 - disc-position) % positions

(defn correct-position [t offset positions disc-position]
  (= (mod (+ t offset) positions)
     (mod (- disc-position) positions)))

(defn solve-one [] (filter (fn [t] (and (correct-position t 1 13 1)
                                        (correct-position t 2 19 10)
                                        (correct-position t 3 3 2)
                                        (correct-position t 4 7 1)
                                        (correct-position t 5 5 3)
                                        (correct-position t 6 17 5))) (range (inc cycle-length))))

(defn solve-two [] (filter (fn [t] (and (correct-position t 1 13 1)
                                        (correct-position t 2 19 10)
                                        (correct-position t 3 3 2)
                                        (correct-position t 4 7 1)
                                        (correct-position t 5 5 3)
                                        (correct-position t 6 17 5)
                                        (correct-position t 7 11 0))) (range (inc cycle-length-two))))
