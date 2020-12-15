(ns galuque.aoc-2020.day-8
  (:require [clojure.java.io :as io]))

(def sample-input
  (slurp (io/resource "day_8/sample_input.txt")))

(def input
  (slurp (io/resource "day_8/input.txt")))

(defn parse-input [input]
  (vec (for [[_ op arg] (re-seq #"(\w{3}) ([+-]\d+)" input)]
         [(keyword op) (read-string arg)])))

;; part 1
(defn run-program [input]
  (let [program (parse-input input)
        init-state {:pc 0
                    :acc 0
                    :seen? #{}}]
    (loop [{:keys [pc acc seen?] :as state} init-state]
      (if (seen? pc)
        acc
        (let [[op arg] (get program pc)]
          (case op
            :nop
            (recur (-> state
                       (update :pc inc)
                       (update :seen? conj pc)))
            :acc
            (recur (-> state
                       (update :acc + arg)
                       (update :pc inc)
                       (update :seen? conj pc)))
            :jmp
            (recur (-> state
                       (update :pc + arg)
                       (update :seen? conj pc)))))))))

(run-program input)
;; => 1949

;; part 2
(defn run-program-2 [program]
  (let [init-state {:pc 0
                    :acc 0
                    :seen? #{}}
        total-ins (count program)]
    (loop [{:keys [pc acc seen?] :as state} init-state]
      (cond
        (seen? pc)
        :infinite-loop

        (= pc total-ins)
        acc

        :else
        (let [[op arg] (get program pc)]
          (case op
            :nop
            (recur (-> state
                       (update :pc inc)
                       (update :seen? conj pc)))
            :acc
            (recur (-> state
                       (update :acc + arg)
                       (update :pc inc)
                       (update :seen? conj pc)))
            :jmp
            (recur (-> state
                       (update :pc + arg)
                       (update :seen? conj pc)))))))))

(defn find-faulty-op
  [input]
  (let [program (parse-input input)]
    (for [i (range (count program))
          :when (#{:nop :jmp} (get-in program [i 0]))
          :let [program (update-in program [i 0] {:jmp :nop, :nop :jmp})
                result (run-program-2 program)]
          :when (not= :infinite-loop result)]
      result)))

(find-faulty-op input)
;; => (2092)
