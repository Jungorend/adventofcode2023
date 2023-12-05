(ns day-four
  (:require [clojure.string :as s]))

(def input (slurp "inputs/dayfour.txt"))

(defn calculate-value [value]
  (cond (= 0 value) 0
        (= 1 value) 1
        :else (* 2 (calculate-value (dec value))))) ; This could blow out the stack if numbers were higher

(defn calculate-game [[id lucky-numbers actual-numbers]]
  (let [winning-numbers (reduce #(conj %1 (Integer/parseInt %2)) #{}
                                (filter seq (s/split lucky-numbers #" ")))
        numbers (map #(Integer/parseInt %) (filter seq (s/split actual-numbers #" ")))
        matching (filter #(winning-numbers %) numbers)]
    (calculate-value (count matching))))

(defn part-one [input]
  (reduce #(+ %1 (calculate-game %2))
          0
          (partition 3
                     (-> input
                         (s/replace "\n" "| ")
                         (s/replace ":" "|")
                         (s/split #"\| ")))))
