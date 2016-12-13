(ns advent-of-code.day-twelve
  (:require [clojure.string :as str]))

(def copy-matcher #"(cpy) ([a-d]|\d+) ([a-d])")

(def inc-matcher #"(inc) ([a-d])")

(def dec-matcher #"(dec) ([a-d])")

(def jnz-matcher #"(jnz) ([a-d]|\d+) (-?\d+)")

(def opcode-matcher #"(cpy|inc|dec|jnz)")

(def match-register #"[a-d]")

(def match-number #"\d+")

(defn register-or-number [s]
  (if (re-find match-register s) (keyword s)
      (Integer/parseInt s)))

(defmulti read-instruction (fn [s] (last (re-find opcode-matcher s))))

(defmethod read-instruction "cpy" [s]
  (let [[_ _ a1 a2] (re-find copy-matcher s)]
    {:opcode :cpy :arg1 (register-or-number a1) :arg2 (register-or-number a2)}))

(defmethod read-instruction "inc" [s]
  (let [[_ _ r] (re-find inc-matcher s)]
    {:opcode :inc :arg1 (register-or-number r)}))

(defmethod read-instruction "dec" [s]
  (let [[_ _ r] (re-find dec-matcher s)]
    {:opcode :dec :arg1 (register-or-number r)}))

(defmethod read-instruction "jnz" [s]
  (let [[_ _ a1 a2] (re-find jnz-matcher s)]
    {:opcode :jnz :arg1 (register-or-number a1) :arg2 (register-or-number a2)}))

(defmulti exec-instruction (fn [env instruction] (:opcode instruction)))

(defmethod exec-instruction :cpy [env instruction]
  (let [{:keys [arg1 arg2]} instruction
        arg1 (if (number? arg1) arg1 (get env arg1))]
    (-> env
        (update :pc inc)
        (assoc arg2 arg1))))

(defmethod exec-instruction :inc [env instruction]
  (let [{:keys [arg1]} instruction]
    (-> env
        (update :pc inc)
        (update arg1 inc))))

(defmethod exec-instruction :dec [env instruction]
  (let [{:keys [arg1]} instruction]
    (-> env
        (update :pc inc)
        (update arg1 dec))))

(defmethod exec-instruction :jnz [env instruction]
  (let [{:keys [arg1 arg2]} instruction
        arg1 (if (number? arg1) arg1 (get env arg1))]
    (cond-> env
      (zero? arg1) (update :pc inc)
      (pos? arg1) (update :pc + arg2))))

(defn solve-one [input]
  (let [instructions (map read-instruction (str/split-lines input))]
    (loop [env {:pc 0 :a 0 :b 0 :c 0 :d 0}]
      (if-let [next-instruction (first (drop (:pc env) instructions))]
        (recur (exec-instruction env next-instruction))
        env))))


(defn solve-two [input]
  (let [instructions (map read-instruction (str/split-lines input))]
    (loop [env {:pc 0 :a 0 :b 0 :c 1 :d 0}]
      (if-let [next-instruction (first (drop (:pc env) instructions))]
        (recur (exec-instruction env next-instruction))
        env))))
