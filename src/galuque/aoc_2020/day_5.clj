(ns galuque.aoc-2020.day-5
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))


(def sample-pass "FBFBBFFRLR")

(def input
  (line-seq (io/reader (io/resource "day_5/input.txt"))))


(defn parse-pass
  [pass]
  (let [[_ row col]
        (re-find #"([F|B]{7})([R|L]{3})" pass)]
    [row col]))

(defn middle
  [beg end]
  (int (Math/ceil (/ (- end beg) 2))))

(defn decode
  [code end lower-half upper-half]
  (loop [beg 0
         end end
         code code]
    (if (nil? code)
      end
      (recur
       (if (= lower-half (first code))
         beg
         (+ beg (middle beg end)))
       (if (= upper-half (first code))
         end
         (- end (middle beg end)))
       (next code)))))

(defn pass-id
   [pass]
   (let [[row col] (parse-pass pass)]
     (+ (* (decode row 127 \F \B) 8)
        (decode col 7 \L \R))))

;; part 1
(apply max (map pass-id input))
;; => 938

;; part 2
(apply min (map pass-id input))

(def real-passes (map pass-id input))

(def complete-passes
  (range (apply min (map pass-id input))
         (apply max (map pass-id input))))

(set/difference (set complete-passes)
                (set real-passes))
;; => #{696}



