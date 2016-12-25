(ns advent-of-code.assembunny)

(def copy-matcher #"(cpy) ([a-d]|-?\d+) ([a-d])")

(def inc-matcher #"(inc) ([a-d])")

(def dec-matcher #"(dec) ([a-d])")

(def mul-matcher #"(mul) ([a-d]) ([a-d]|\d+)")

(def jnz-matcher #"(jnz) ([a-d]|\d+) ([a-d]|-?\d+)")

(def tgl-matcher #"(tgl) ([a-d]|\d+)")

(def out-matcher #"(out) ([a-d]|-?\d+)")

(def opcode-matcher #"(cpy|inc|dec|jnz|tgl|mul|nop|out)")

(def match-register #"[a-d]")

(def match-number #"\d+")

(defn register-or-number [s]
  (if (re-find match-register s) (keyword s)
      (Integer/parseInt s)))

(defmulti read-instruction (fn [s]
                             (last (re-find opcode-matcher s))))

(defmethod read-instruction "cpy" [s]
  (let [[_ _ a1 a2] (re-find copy-matcher s)]
    {:opcode :cpy :arg1 (register-or-number a1) :arg2 (register-or-number a2)}))

(defmethod read-instruction "nop" [s]
  {:opcode :nop})

(defmethod read-instruction "inc" [s]
  (let [[_ _ r] (re-find inc-matcher s)]
    {:opcode :inc :arg1 (register-or-number r)}))

(defmethod read-instruction "mul" [s]
  (let [[_ _ a1 a2] (re-find mul-matcher s)]
    {:opcode :mul :arg1 (register-or-number a1) :arg2 (register-or-number a2)}))

(defmethod read-instruction "dec" [s]
  (let [[_ _ r] (re-find dec-matcher s)]
    {:opcode :dec :arg1 (register-or-number r)}))

(defmethod read-instruction "jnz" [s]
  (let [[_ _ a1 a2] (re-find jnz-matcher s)]
    {:opcode :jnz :arg1 (register-or-number a1) :arg2 (register-or-number a2)}))

(defmethod read-instruction "tgl" [s]
  (let [[_ _ a1] (re-find tgl-matcher s)]
    {:opcode :tgl :arg1 (register-or-number a1)}))

(defmethod read-instruction "out" [s]
  (let [[_ _ a1] (re-find out-matcher s)]
    {:opcode :out :arg1 (register-or-number a1)}))

(defmulti exec-instruction (fn [env instruction] (:opcode instruction)))

(defmethod exec-instruction :nop [env instruction]
  (update env :pc inc))

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

(defmethod exec-instruction :mul [env instruction]
  (let [{:keys [arg1 arg2]} instruction
        arg2 (if (number? arg2) arg2 (get env arg2))]
    (-> env
        (update :pc inc)
        (update arg1 * arg2))))

(defmethod exec-instruction :dec [env instruction]
  (let [{:keys [arg1]} instruction]
    (-> env
        (update :pc inc)
        (update arg1 dec))))

(defmethod exec-instruction :out [env instruction]
  (let [{:keys [arg1]} instruction
        arg1 (if (number? arg1) arg1 (get env arg1))]
    (-> env
        (update :pc inc)
        (update :out conj arg1))))

(defmethod exec-instruction :jnz [env instruction]
  (let [{:keys [arg1 arg2]} instruction
        arg1 (if (number? arg1) arg1 (get env arg1))
        arg2 (if (number? arg2) arg2 (get env arg2))]
    (cond-> env
      (zero? arg1) (update :pc inc)
      (pos? arg1) (update :pc + arg2))))
