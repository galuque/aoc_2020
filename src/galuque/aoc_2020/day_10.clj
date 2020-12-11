(ns galuque.aoc-2020.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def small-input
  (map #(Long/parseLong %)
       (str/split-lines (slurp (io/resource "day_10/small_input.txt")))))


(def sample-input
  (map #(Long/parseLong %) 
       (str/split-lines (slurp (io/resource "day_10/sample_input.txt")))))

(def input
  (map #(Long/parseLong %)
       (str/split-lines (slurp (io/resource "day_10/input.txt")))))

;; part 1
(defn jolts-product
  [input]
  (let [device-jolt (+ 3 (apply max input))]
    (->> (conj input 0 device-jolt)
         sort
         (partition 2 1)
         (map (partial apply -))
         frequencies
         vals
         (apply *))))

(jolts-product input)
;; => 2100

;; part 2


#_(loop [options (sort (conj small-input  (+ 3 (apply max small-input))))
       qty 0
       root 0]
  (let [result (for [x options
                     :while (<= (- x root) 3)]
                 x)]
    (prn result qty root options)
    (if (not (seq options))
      qty
      (recur
       (next options)
       (+ qty (count result))
       (first options)))))


(defn remove-index [v ^long idx]
  (into (subvec v 0 idx) (subvec v (inc idx) (count v))))

(defn can-be-removed? [v ^long idx]
  (let [left (nth v (dec idx))
        right (nth v (inc idx))]
    (and left right (<= (- (long right) (long left)) 3))))

(defn add-boundaries [input]
  (vec (sort (conj input 0 (+ 3 (long (apply max input)))))))


(defn combos ^long [input ^long start]
  (loop [cnt 1
         idx start]
    (cond
      (<= (count input) 2)
      cnt

      (<= (dec (count input)) idx)
      cnt

      (can-be-removed? input idx)
      (let [removed (remove-index input idx)]
        (recur (+ cnt (combos removed idx))
               (inc idx)))

      :else
      (recur cnt (inc idx)))))


(defn divide [input]
  (let [split-points (remove #(can-be-removed? input %) (range 1 (dec (count input))))]
    (if (seq split-points)
      (let [split-idx (long (nth split-points (quot (count split-points) 2)))
            left (take (inc split-idx) input)
            right (drop split-idx input)]
        [left right])
      (combos (vec input) 1))))

(defn conquer [input]
  (let [parts (divide input)]
    (if (number? parts)
      parts
      (apply * (map conquer parts)))))


(conquer (add-boundaries input))
;; => 16198260678656
