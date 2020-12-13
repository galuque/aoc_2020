(ns galuque.aoc-2020.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
  (slurp (io/resource "day_13/sample_input.txt")))

(def input
  (slurp (io/resource "day_13/input.txt")))

(defn parse-input
  [input]
  (let [[min-ts buses-str] (str/split-lines input)]
    [(read-string min-ts)
     (->> (str/split buses-str #",")
          (map read-string)
          (filter number?))]))

(defn part1
  [input]
  (let [[min-ts buses] (parse-input input)
        options (for [bus buses
                      :let [diff (- bus (mod min-ts bus))]]
                  [bus diff])]
    (->> options
         (sort-by second)
         first
         (apply *))))

(part1 input)
;; => 5946

;; part 2

(defn parse-input-2 [input]
  (->> input
       (re-seq #"[\dx]+")
       (next)
       (map read-string)
       (map-indexed (fn [i n]
                      (when (not= 'x n)
                        [i n])))
       (filter some?)))

(defn bus-cycle
  [[idx bus]]
  (iterate #(+ bus %) (- idx)))

(defn narrow-cycle [bc [^long idx ^long bus]]
  (let [new-cycle (filter #(= 0 (mod (+ ^long % idx) bus)) bc)
        [n1 n2] (take 2 new-cycle)
        diff (- ^long n2 ^long n1)]
    (iterate #(+ ^long % diff) n1)))

(defn part2
  [input]
  (let [parsed-input (parse-input-2 input)]
    (first
     (let [[s & ss] (sort-by second parsed-input)]
       (reduce narrow-cycle (bus-cycle s) ss)))))

(part2 input)
;; => 645338524823718
