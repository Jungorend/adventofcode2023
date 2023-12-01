(ns day-one
  (:require [clojure.string :as s]))

(def input (slurp "inputs/dayone.txt"))

(defn extract-numbers
  "takes a string and returns a list of each line. Each line only includes each individual number"
  [string]
  (map (fn [line]
         (filter #{\1 \2 \3
                   \4 \5 \6
                   \7 \8 \9}
                 (seq line)))
       (s/split string #"\n")))

(defn sum-calibration-numbers
  "Returns the sum of all the calibration numbers on each line."
  [string]
  (reduce (fn [sum line]
            (+ sum (Integer/parseInt (str (first line) (last line)))))
          0
          (extract-numbers string)))

(def word->number
  {#"one" "o1e"
   #"two" "t2o"
   #"three" "t3e"
   #"four" "f4r"
   #"five" "f5e"
   #"six" "s6x"
   #"seven" "s7n"
   #"eight" "e8t"
   #"nine" "n9e"
   #"zero" "z0o"})

(defn convert-words
  "Takes all 'word numbers' and converts them to letters"
  [string]
  (reduce (fn [str key]
            (s/replace str (first key) (second key)))
          string
          word->number))

;; Part One
; (sum-calibration-numbers input)
;
; => 55130

;; Part Two
;; (sum-calibration-numbers (convert-words input))
;;
;; => 54985
