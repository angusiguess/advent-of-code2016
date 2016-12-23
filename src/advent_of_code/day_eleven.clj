(ns advent-of-code.day-eleven
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as pm]))

;; The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
;; The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
;; The third floor contains nothing relevant.
;; The fourth floor contains nothing relevant.

(def init-state {1 #{[:po :gen] [:thu :gen] [:thu :chip] [:pro :gen] [:rut :gen]
                     [:rut :chip] [:cob :gen] [:cob :chip]}
                 2 #{[:po :chip] [:pro :chip]}
                 3 #{}
                 4 #{}})

(def init-state-two (-> init-state
                        (update 1 conj [:el :gen])
                        (update 1 conj [:el :chip])
                        (update 1 conj [:di :gen])
                        (update 1 conj [:di :chip])))

(defn goal? [state]
  (and (empty? (get state 1))
       (empty? (get state 2))
       (empty? (get state 3))))

;; A floor has a conflict if a chip doesn't have its corresponding generator AND
;; some other generator is on that floor

(defn valid-floor? [floor]
  (let [chips (into #{} (comp
                         (filter (fn [[_ type]] (= type :chip)))
                         (map first)) floor)
        gens (into #{} (comp
                        (filter (fn [[_ type]] (= type :gen)))
                        (map first)) floor)]
    (or (empty? gens)
        (empty? chips)
        (set/difference chips gens))))

(defn heuristic [state]
  (+ (* 3 (count (get state 1)))
     (* 2 (count (get state 2)))
     (* 1 (count (get state 3)))))

(defn valid? [state]
  (and (valid-floor? (get state 1))
       (valid-floor? (get state 2))
       (valid-floor? (get state 3))
       (valid-floor? (get state 4))))

(defn dest-floor [floor]
  (case floor
    1 [2]
    2 [1 3]
    3 [2 4]
    4 [3]))

(defn gen-moves [floor state]
  (into #{}
        (for [first-move (get state floor)
              second-move (conj (get state floor) [:nothing :nothing])
              next-floor (dest-floor floor)
              :when (not= first-move second-move)]
          [#{first-move second-move} next-floor])))

(defn apply-move [state prev-floor floor first second]
  (cond-> state
    (not= [:nothing :nothing] first) (update prev-floor disj first)
    (not= [:nothing :nothing] first) (update floor conj first)
    (not= [:nothing :nothing] second) (update prev-floor disj second)
    (not= [:nothing :nothing] second) (update floor conj second)))

(defn next-moves [floor state seen]
  (let [moves (gen-moves floor state)]
    (into [] (comp (map (fn [[moves next-floor]]
                          (let [[first second] (seq moves)]
                            [next-floor (apply-move state floor next-floor first second)])))
                   (filter (fn [x] (valid? (last x))))
                   (filter #(nil? (get seen %)))) moves)))

(defn enqueue [q to-enqueue]
  (reduce (fn [acc [floor depth state :as x]]
            (assoc acc x (heuristic state))) q to-enqueue))

(defn bfs [init-state]
  (loop [q (pm/priority-map [1 0 init-state] (heuristic init-state))
         seen #{}]
    (let [[first & rest] q
          [[floor depth state] priority] first
          to-enqueue (next-moves floor state seen)
          with-depth (map (fn [[floor state]]
                            [floor (inc depth) state])
                          to-enqueue)]
      (cond (goal? state) [depth state]
            :else (recur (enqueue (dissoc q [floor depth state]) with-depth)
                         (into seen to-enqueue))))))
