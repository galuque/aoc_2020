(ns galuque.aoc-2020.day-11
   (:require [clojure.java.io :as io]
             [clojure.string :as str]))

(def sample-input
  (slurp (io/resource "day_11/sample_input.txt")))

(def input
  (slurp (io/resource "day_11/input.txt")))

(defn get-layout
  [input]
  (mapv #(mapv '{\. _ \L L} %)
        (str/split-lines input)))

(defn get-max-y
  [layout]
  (dec (count (first layout))))

(defn get-max-x
  [layout]
  (dec (count layout)))

;; part 1

(defn adjacent [x y max-x max-y]
  (for [x'(range (max 0 (dec x)) (inc (min max-x (inc x))))
        y' (range (max 0 (dec y)) (inc (min max-y (inc y))))
        :when (not (and (= x x') (= y y')))]
    [x' y']))

(defn adj-count
  [layout x y max-x max-y]
    (reduce (fn [qty coords]
              (+ qty (if (= 'O (get-in layout coords)) 1 0)))
            0
            (adjacent x y max-x max-y)))

(defn generation
  [layout max-x max-y]
  (mapv (fn [x]
          (mapv (fn [y]
                  (case (get-in layout [x y])
                    _ '_
                    L (if (= 0 (adj-count layout x y max-x max-y)) 'O 'L)
                    O (if (< (adj-count layout x y max-x max-y) 4) 'O 'L)))
                (range (inc max-y))))
        (range (inc max-x))))

(defn find-fixed-layout
  [f layout]
  (let [max-x (get-max-x layout)
        max-y (get-max-y layout)]
    (loop [layout layout
           layout' (f layout max-x max-y)]
      (if (= layout layout')
        layout
        (recur
         layout'
         (f layout' max-x max-y))))))

(defn occupied-seats
  [f input]
  (let [last-layout (find-fixed-layout f (get-layout input))
        xf (comp cat (map  '{_ 0 L 0 O 1}))]
    (transduce xf
               +
               last-layout)))

(occupied-seats generation input)
;; => 2194

;; part 2

(def directions (for [x (range -1 2)
                      y (range -1 2)
                      :when (not= [x y] [0 0])]
                  [x y]))

(defn seat-in-direction [layout x y max-x max-y [dx dy]]
  (loop [x (+ x dx)
         y (+ y dy)]
    (if (or (< x 0) (< y 0) (< max-x x) (< max-y y))
      0
      (let [seat (get-in layout [x y])]
        (case seat
          L 0
          O 1
          _ (recur (+ x dx)
                   (+ y dy)))))))

(defn compute-seat [layout x y max-x max-y]
  (reduce
   (fn [acc dir]
     (let [acc (+ acc (seat-in-direction layout x y max-x max-y dir))]
       (if (= acc 5)
         (reduced 5)
         acc)))
   0
   directions))

(defn generation-2
  ([layout max-x max-y]
   (mapv (fn [x]
           (mapv (fn [y]
                   (let [sym (get-in layout [x y])]
                     (if (= '_ sym)
                       '_
                       (case (compute-seat layout x y max-x max-y)
                         5 'L
                         0 'O
                         sym))))
                 (range (inc max-y))))
         (range (inc max-x)))))


(occupied-seats generation-2 input)
;; => 1944

