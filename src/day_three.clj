(ns day-three
  (:require [clojure.string :as s]))

(def input (slurp "inputs/daythree.txt"))

(defn parse-parts-structure [string]
  (loop [string string
         position [0 0]
         results {:symbols {}
                  :parts-locations {}
                  :part-numbers {}}]
    (cond (empty? string) results
          (= (first string) \.) (recur (rest string)
                                        (assoc position 0 (inc (get position 0)))
                                        results)
          (= (first string) \newline) (recur (rest string)
                                         [0 (inc (get position 1))]
                                         results)
          (re-find #"^\d" (apply str string))  (let [number (re-find #"\d+" (apply str string))
                                                     positions (map #(vector % (get position 1))
                                                                    (range (get position 0) (+ (get position 0) (count number))))
                                                     sym (gensym)]
                                                 (recur (drop (count number) string)
                                                        (assoc position 0 (+ (count number) (get position 0)))
                                                        (-> (reduce (fn [m v]
                                                                      (assoc-in m [:parts-locations v] sym))
                                                                    results
                                                                    positions)
                                                            (assoc-in [:part-numbers sym] number))))
          :else (recur (rest string)
                       (assoc position 0 (inc (get position 0)))
                       (assoc-in results [:symbols position] (first string))))))

(defn valid-part-numbers [parts-structure]
  (reduce (fn [neighbored-parts symbol]
            (let [[x y] (first symbol)
                  neighbors [[(- x 1) (- y 1)] [x (- y 1)] [(+ x 1) (- y 1)]
                             [(- x 1) y] [(+ x 1) y]
                             [(- x 1) (+ y 1)] [x (+ y 1)] [(+ x 1) (+ y 1)]]
                  parts (map #(get-in parts-structure [:parts-locations %]) neighbors)]
              (apply conj neighbored-parts (filter identity parts))))
          #{}
          (:symbols parts-structure)))

(defn gear-ratio-sum [parts-structure]
  (reduce (fn [sum symbol]
            (let [[x y] (first symbol)
                  neighbors [[(- x 1) (- y 1)] [x (- y 1)] [(+ x 1) (- y 1)]
                             [(- x 1) y] [(+ x 1) y]
                             [(- x 1) (+ y 1)] [x (+ y 1)] [(+ x 1) (+ y 1)]]
                  parts (map #(get-in parts-structure [:parts-locations %]) neighbors)
                  valid-parts (disj (into #{} parts) nil)]
              (if (= (count valid-parts) 2)
                (+ sum (* (Integer/parseInt (get-in parts-structure [:part-numbers (first valid-parts)]))
                          (Integer/parseInt (get-in parts-structure [:part-numbers (second valid-parts)]))))
                sum)))
          0
          (filter #(= \* (second %)) (:symbols parts-structure))))

(defn part-one [string]
  (let [structure (parse-parts-structure string)]
    (reduce #(+ %1 (Integer/parseInt (get-in structure [:part-numbers %2])))
            0
            (valid-part-numbers structure))))

(defn part-two [string]
  (gear-ratio-sum (parse-parts-structure string)))
