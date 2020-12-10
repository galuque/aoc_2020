(ns galuque.aoc-2020.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input
  (mapv #(Long/parseLong %) (str/split-lines (slurp (io/resource "day_9/sample_input.txt")))))

(def input
  (mapv #(Long/parseLong %) (str/split-lines (slurp (io/resource "day_9/input.txt")))))


(defn parse-input
  [preamble input]
  (->> input
       (partition-all (inc preamble) 1)
       (take-while #(= (inc preamble) (count %)))
       (map vec)
       vec))


(defn valid-nums
  [pream coll]
  (let [preamble (take pream coll)
        n (last coll)
        pairs (for [x preamble
                    y preamble
                    :while (not= x y)
                    :let [result (+ x y)]
                    :when (= result n)]
                result)]
    (vec pairs)))


(defn valid-set
  [preamble input]
  (set (apply concat
              (map #(valid-nums preamble %)
                   (parse-input preamble input)))))

(defn input-set
  [preamble input]
  (set (subvec input preamble)))

(defn find-invalid 
  [preamble input]
  (set/difference
   (input-set preamble input)
   (valid-set preamble input)))

(find-invalid 25 input)
;; => #{552655238}

