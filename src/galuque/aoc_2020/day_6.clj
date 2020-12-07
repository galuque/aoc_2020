(ns galuque.aoc-2020.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input "abc

a
b
c

ab
ac

a
a
a
a

b")

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

(transduce (map count-answers-2)
           +
           input)
;; => 3640
