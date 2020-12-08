(ns galuque.aoc-2020.day-7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-input
  (line-seq (io/reader (io/resource "day_7/sample_input.txt"))))

(def sample-input-2
  (line-seq (io/reader (io/resource "day_7/sample_input_2.txt"))))

(def input
  (line-seq (io/reader (io/resource "day_7/input.txt"))))


(defn parse-entry [s]
  (let [[bag & deps] (str/split s #"\s?(contain|,)\s?")
        color (re-find #"\w+ \w+" bag)]
    [color (keep (comp next (partial re-find #"(\d+) (\w+ \w+)")) deps)]))

;; part 1
(defn color-graph
  [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [_ col]]
                      (update m col conj bag))
                    m deps))
          {}
          entries))

(defn add-valid
  [result graph color]
  (into result (get graph color)))

(defn valid-outer
  [graph start]
  (loop [result (add-valid #{} graph start)]
    (let [result2 (reduce (fn [res color]
                            (add-valid res graph color))
                          result
                          result)]
      (if (= result result2)
        result
        (recur result2)))))

(count
 (valid-outer (color-graph (map parse-entry input)) "shiny gold"))
;; => 148

;; part 2
(defn nesting-graph
  [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m bag conj [(Long/parseLong num) col]))
                    m deps))
          {}
          entries))

(defn color-count
  [graph color]
  (let [entries (get graph color)]
    (if (seq entries)
      (reduce
       (fn [cant [num color]]
         (+ cant (* num (color-count graph color))))
       1
       entries)
      1)))

(def graph (nesting-graph (map parse-entry input)))

(dec (color-count graph "shiny gold"))
;; => 24867
