(ns day-five
  (:require [clojure.string :as s]))

(def input (slurp "inputs/dayfive.txt"))

(defn create-mappings [input]
  (reduce (fn [m string]
            (let [[definition values] (s/split string #":[\n ]")]
              (if (= "seeds" definition)
                (assoc m :seed (map parse-long (s/split values #" ")))
                (assoc m (keyword (first (s/split definition #" ")))
                       (map (fn [rang]
                              (let [[fst snd trd] (s/split rang #" ")]
                                {:dest (parse-long fst)
                                 :source (parse-long snd)
                                 :range (parse-long trd)})) (s/split values #"\n"))))))
          {}
          (s/split input #"\n\n")))

(defn transform [mappings value]
  (let [transformations (map (fn [m]
                               (let [distance (- value (:source m))]
                                 (if (and (>= value (:source m)) (<= distance (:range m)))
                                   (+ (:dest m) distance)
                                   nil)))
                             mappings)
        valid-transformations (filter identity transformations)]
    (if (empty? valid-transformations)
      value
      (first valid-transformations))))

(defn get-location [mapping]
  (apply min
         (reduce (fn [values conversions]
                   (map #(transform (conversions mapping) %) values))
                 (:seed mapping)
                 [:seed-to-soil :soil-to-fertilizer :fertilizer-to-water :water-to-light
                  :light-to-temperature :temperature-to-humidity :humidity-to-location])))

(def part-one
  (get-location (create-mappings input)))

(defn calculate-seeds
  [seeds]
  (reduce (fn [r s]
            (apply conj r (range (first s) (+ (second s) (first s)))))
          []
          (partition 2 seeds)))
