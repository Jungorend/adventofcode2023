(ns day-five
  (:require [clojure.string :as s]))

(def input (slurp "inputs/dayfive.txt"))

(defn create-mappings [input]
  (reduce (fn [m string]
         (let [[definition values] (s/split string #":[\n ]")]
           (if (= "seeds" definition)
             (assoc m :seed (map parse-long (s/split values #" ")))
             (assoc-in
              (assoc m (keyword (first (s/split definition #" ")))
                     (map (fn [rang]
                            (let [[fst snd trd] (s/split rang #" ")]
                              {:dest (parse-long fst)
                               :source (parse-long snd)
                               :range (parse-long trd)})) (s/split values #"\n")))
              [:schema (keyword (first (s/split definition #"-")))]
              (keyword (first (s/split definition #" ")))))))
          {}
       (s/split input #"\n\n")))
