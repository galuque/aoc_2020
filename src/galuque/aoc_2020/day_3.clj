(ns galuque.aoc-2020.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def input
  (slurp (io/resource "day_3/input.txt")))

(defn input->map
  [input]
  (mapv (fn [row]
          (mapv {\# true \. false} row))
        (str/split-lines input)))

(defn tree?
  [m x y]
  (let [width (count (first m))]
    (get-in m [y (mod x width)])))

(defn sled [[down-x down-y] [my-map x y trees]]
  (let [x (+ x down-x)
        y (+ y down-y)
        tree? (tree? my-map x y)]
    (cond
      (nil? tree?)
      (reduced trees)
      
      (true? tree?)
      [my-map x y (inc trees)]
      
      :else
      [my-map x y trees])))

(defn sled-down
  [slope input]
  (let [tree-map (input->map input)]
    @(first
      (drop-while
       (complement reduced?)
       (iterate (partial sled slope) [tree-map 0 0 0])))))


(sled-down [3 1] sample-input)
;; => 7

;; part 1
(sled-down [3 1] input)
;; => 145

;; part 2

(def slopes [[1 1]
             [3 1]
             [5 1]
             [7 1]
             [1 2]])

(apply * (for [s slopes]
           (sled-down s input)))
;; => 3424528800


