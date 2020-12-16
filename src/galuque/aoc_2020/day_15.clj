(ns galuque.aoc-2020.day-15
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def sample-input
   (mapv #(Long/parseLong %) (str/split "0,3,6" #",")))

(def input
  (map #(Long/parseLong %) (str/split "8,11,0,19,1,2" #",")))

(defn input->map [input]
  (apply hash-map (interleave input (map inc (range)))))

(defn number-spoken-at [pos input]
  (let [state {:seen? (set input)
               :turns-spoken (input->map input)}]
    (loop [state state
           last-spoken 0
           turn (inc (count input))]
      (if (= turn pos)
        last-spoken
        (if ((:seen? state) last-spoken)
          (recur
           (assoc-in state [:turns-spoken (long last-spoken)] turn)
           (long (unchecked-subtract-int turn (get-in state [:turns-spoken last-spoken])))
           (inc turn))
          (recur
           (-> state
               (assoc-in [:seen?] (conj (:seen? state) last-spoken))
               (assoc-in [:turns-spoken last-spoken] turn))
           0
           (inc turn)))))))

(number-spoken-at 2020 input)
;; => 447

;; part 2
;; takes too long
(time
 (number-spoken-at 30000000 input))
;; => 11721679
;; "Elapsed time: 35779.42395 msecs"
