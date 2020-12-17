(ns galuque.aoc-2020.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
  (slurp (io/resource "day_16/sample_input.txt")))

(def input
  (slurp (io/resource "day_16/input.txt")))

(defn to-range
  [str-range]
  (let [[low hi]  (str/split str-range #"-")]
    (range (read-string low) (inc (read-string hi)))))

(defn range-set [r1 r2]
  (set (reduce
        conj
        (to-range r1)
        (to-range r2))))


(defn input-sets [input]
  (let [match  #"(\w+):[ \n](\d+-\d+) \w+ (\d+-\d+)"
        in     (->> (str/split input #"\n\n")
                    first
                    (re-seq match))]
    (for [[_ _ range1 range2] in]
      (range-set range1 range2))))

(defn get-tickets-vals [input]
  (-> input
      (str/split #"\n\n")
      (nth 2)
      (str/split-lines)
      rest
      (->>
       (map #(str/split % #","))
       (map #(map read-string %))
       (reduce concat))))

(defn invalid-fields [input]
  (let [ss              (input-sets input)
        in-any-set?     (apply juxt ss)
        ticket-in-set? #(some some? (in-any-set? %))
        ticket-vals     (get-tickets-vals input)]
    (filter 
     (complement ticket-in-set?) 
     ticket-vals)))

(defn error-rate
  [input]
  (reduce 
   + 
   (invalid-fields input)))

(error-rate input)
;; => 24021


;; part 2