(ns galuque.aoc-2020.day-5
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def sample-pass "FBFBBFFRLR")

(def input
  (line-seq (io/reader (io/resource "day_5/input.txt"))))

(def encoding {\F 0 \B 1 \L 0 \R 1})

(defn chars->num [chars]
  (Long/parseLong (reduce str (map encoding chars)) 2))

(defn seat-id [pass]
  (let [[row col] (partition-all 7 pass)]
    (+ (chars->num col)
       (* 8 (chars->num row)))))

;; part 1
(apply max (map seat-id input))
;; => 938

;; part 2
(apply min (map seat-id input))

(def real-passes (map seat-id input))

(def complete-passes
  (range (apply min (map seat-id input))
         (apply max (map seat-id input))))

(set/difference (set complete-passes)
                (set real-passes))
;; => #{696}
