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
  (slurp (io/resource "day_6/input.txt")))

(defn parse-input
  [input]
  (-> input
   (str/split #"\n\n")
   (->> 
    (map str/split-lines))))

(defn count-answers
  [answers]
  (-> answers
      str/join
      set
      count))

;; part 1
(apply +
       (map count-answers
            (parse-input input)))
;; => 7128

;; part 2
(defn count-answers-2
  [answers]
  (->> answers
       (map set)
       (reduce set/intersection)
       count))

 (apply +
        (map count-answers-2
             (parse-input input)))
;; => 3640

