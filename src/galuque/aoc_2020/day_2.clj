(ns galuque.aoc-2020.day-2
  (:require [clojure.java.io :as io]))

(def sample-input  "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(def re #"(\d+)-(\d+) (.): (.*)")

(defn parse-line [s]
  (let [[_ min max char passw] (re-find re s)]
    [(Long/parseLong min) (Long/parseLong max) (first char) passw]))

(defn passw-ok? [[min max char passw]]
  (<= min (get (frequencies passw) char 0) max))

(def input 
  (map parse-line (line-seq (io/reader (io/resource "day_2/input.txt")))))

;; part 1
(count
 (filter passw-ok? input))
;; => 445

;;part 2
(defn passw-ok-2? [[min max char passw]]
  (let [op1 (= (nth passw (dec min)) char)
        op2 (= (nth passw (dec max)) char)]
    (not= op1 op2)))

(count
 (filter passw-ok-2? input))
;; => 491
