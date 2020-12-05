(ns galuque.aoc-2020.day-1
  (:require [clojure.java.io :as io]))

(def sample-input [1721
                   979
                   366
                   299
                   675
                   1456])

(def input (map #(Long/parseLong %)
                (line-seq (io/reader (io/resource "day_1/input.txt")))))

;; part 1
(set (for [x input
           y input
           :when (= 2020 (+ x y))]
       (* x y)))
;; => #{145875}
;; => #{514579} ;; Sample answer

;; part 2
(set (for [x input
           y input
           z input
           :when (= 2020 (+ x y z))]
       (* x y z)))
;; => #{69596112}
;; => #{241861950} ;; sample answer
