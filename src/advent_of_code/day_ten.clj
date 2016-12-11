(ns advent-of-code.day-ten
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

;; Parsing logic

(def gives-value #"value (\d+) goes to bot (\d+)")

(def compares-and-gives #"bot (\d+) gives (low|high) to (bot|output) (\d+) and (low|high) to (bot|output) (\d+)")

(defn match-instruction [s]
  (cond (re-find gives-value s) :give-chip
        (re-find compares-and-gives s) :move-chip))

(defmulti read-instruction match-instruction)

(defmethod read-instruction :give-chip [s]
  (let [[_ value bot] (re-find gives-value s)]
    {:type :give-chip
     :value (Integer/parseInt value)
     :bot (Integer/parseInt bot)}))

(defmethod read-instruction :move-chip [s]
  (let [[_ sender comp-one receiver-one-type receiver-one comp-two receiver-two-type receiver-two] (re-find compares-and-gives s)]
    {:type :move-chip
     :sender (Integer/parseInt sender)
     :comp-one (keyword comp-one)
     :receiver-one-type (keyword receiver-one-type)
     :receiver-one (Integer/parseInt receiver-one)
     :comp-two (keyword comp-two)
     :receiver-two-type (keyword receiver-two-type)
     :receiver-two (Integer/parseInt receiver-two)}))

(defn build-rules [rules]
  (reduce (fn [acc {:keys [sender] :as rule}]
            (assoc acc sender rule)) {} rules))

(defn fire-rule [rules env sender]
  (let [{:keys [sender
                comp-one receiver-one-type receiver-one
                comp-two receiver-two-type receiver-two] :as rule} (get rules sender)]
    (if rule
      (let [[low high] (sort (get-in env [:bot sender]))
            to-one (case comp-one :low low :high high)
            to-two (case comp-two :low low :high high)
            next-env (-> env
                         (update-in [:bot sender] disj to-one)
                         (update-in [:bot sender] disj to-two)
                         (update-in [receiver-one-type receiver-one] (fnil conj #{}) to-one)
                         (update-in [receiver-two-type receiver-two] (fnil conj #{}) to-two))]
        (when (= (get-in next-env [receiver-one-type receiver-one]) #{61 17}) (println receiver-one))
        (when (= (get-in next-env [receiver-two-type receiver-two]) #{61 17}) (println receiver-two))
        next-env)
      env)))

(defn should-fire? [env]
  (boolean (some #(= (count %) 2) (vals env))))

(defn sender [env]
  (first (first (filter (fn [[k v]]
                    (= 2 (count v))) env))))

(defn fire-rules [rules env]
  (if (should-fire? (:bot env)) (recur rules (fire-rule rules env (sender (:bot  env))))
      env))

(defn add-chip [env rules {:keys [value bot]}]
  (let [next-env (update-in env [:bot bot] (fnil conj #{}) value)]
    next-env))

(defn solve-one []
  (let [input (slurp (io/file (io/resource "day_ten.txt")))
        lines (->> input
                   str/split-lines
                   (map read-instruction))
        {:keys [give-chip move-chip]} (group-by :type lines)
        rules (build-rules move-chip)
        init-state (reduce (fn [acc give]
                             (add-chip acc rules give)) {} give-chip)]
    (fire-rules rules init-state)))


(defn solve-two []
  (let [input (slurp (io/file (io/resource "day_ten.txt")))
        lines (->> input
                   str/split-lines
                   (map read-instruction))
        {:keys [give-chip move-chip]} (group-by :type lines)
        rules (build-rules move-chip)
        init-state (reduce (fn [acc give]
                             (add-chip acc rules give)) {} give-chip)]
    (-> (fire-rules rules init-state)
        :output
        (select-keys [0 1 2])
        vals)))
