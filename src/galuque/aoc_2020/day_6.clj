(ns galuque.aoc-2020.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input 
  (map str/split-lines (str/split (slurp (io/reader (io/resource "day_6/sample_input.txt"))) #"\n\n")))

(def input
  (map str/split-lines (str/split (slurp (io/resource "day_6/input.txt")) #"\n\n")))

;; part 1
(apply +
       (map (comp count set str/join)
            input))
;; => 7128

;; part 2
(defn count-answers-2
  [answers]
  (->> answers
       (map set)
       (apply set/intersection)
       count))

(reduce
 +
 (map count-answers-2 input))
;; => 3640
