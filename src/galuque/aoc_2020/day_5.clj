(ns galuque.aoc-2020.day-5
  (:require [clojure.java.io :as io]))

(def sample-pass "FBFBBFFRLR")

(def input
  (line-seq (io/reader (io/resource "day_5/input.txt"))))

(def encoding {\F 0 \B 1 \L 0 \R 1})

(defn seat-id [pass]
  (->> pass
       (map encoding)
       (reduce str "2r")
       (read-string)))

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
