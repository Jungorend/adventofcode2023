(ns day-two
  (:require [clojure.string :as s]))

(def input (slurp "inputs/daytwo.txt"))
(def valid-cubes {"red" 12, "green" 13, "blue" 14})

(defn set-valid? [set]
  (let [pulls (->> (s/split set #", ")
                   (map #(s/split % #" ")))]
    (every? (fn [[number color]]
              (if (<= (Integer/parseInt number) (get valid-cubes color))
                true
                false))
            pulls)))

(defn game-valid? [line]
  (every? set-valid? (s/split line #"; ")))

(defn sum-successful-games [string]
  (loop [lines (s/split string #"\n")
         sum 0]
    (if (seq lines)
      (let [game (s/split (first lines) #": ")
            id (Integer/parseInt (second (s/split (first game) #" ")))]
        (recur (rest lines) (if (game-valid? (second game))
                              (+ sum id)
                              sum)))
      sum)))

(defn power-of-game [game]
  (loop [pulls (-> game
                   (s/replace "; " ", ")
                   (s/split #", "))
         colors {"red" 0 "blue" 0 "green" 0}]
    (if (seq pulls)
      (let [[number color] (s/split (first pulls) #" ")]
        (recur (rest pulls)
               (if (> (Integer/parseInt number) (get colors color))
                 (assoc colors color (Integer/parseInt number))
                 colors)))
      (reduce * (vals colors)))))

(defn sum-game-powers [string]
  (let [lines (s/split string #"\n")
        games (map #(second (s/split % #": ")) lines)]
    (reduce #(+ %1 (power-of-game %2)) 0 games)))

(defn -main [& args]
  (println (sum-successful-games input))
  (println (sum-game-powers input)))
