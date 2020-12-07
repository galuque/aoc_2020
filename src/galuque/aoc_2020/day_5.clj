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
(def seat-ids (map seat-id input))
(apply max seat-ids)
;; => 938

;; part 2
(defn skipped?
  [[low high]]
  (when (= high (+ 2 low))
    (inc low)))

(some
 skipped?
 (partition-all 2 1 (sort seat-ids)))
;; => 696
