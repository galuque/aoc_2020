(ns galuque.aoc-2020.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input
  (slurp (io/resource "day_12/small_input.txt")))

(def input
  (slurp (io/resource "day_12/input.txt")))

(defn parse-input [input]
  (vec (for [[_ op arg]  (re-seq #"(\w)(\d+)" input)]
         [(keyword op) (read-string arg)])))

(def turns
  {:N {:R {90 :E
           180 :S
           270 :W}
       :L  {90 :W
            180 :S
            270 :E}}
   :E {:R {90 :S
           180 :W
           270 :N}
       :L  {90 :N
            180 :W
            270 :S}}
   :S {:R {90 :W
           180 :N
           270 :E}
       :L  {90 :E
            180 :N
            270 :W}}
   :W {:R {90 :N
           180 :E
           270 :S}
       :L  {90 :S
            180 :E
            270 :N}}})

(defn move-to-dir!
  [state dir amount]
  (case dir
    :E (update state :x + amount)

    :S (update state :y - amount)

    :W (update state :x - amount)

    :N (update state :y + amount)))

(defn rotate-dir [dir turn deg]
  (-> turns
      dir
      turn
      (get deg)))

;; part 1

(defn get-next-dir
  [state turn deg]
  (let [{:keys [curr-dir]} state
        next-dir (rotate-dir curr-dir turn deg)]
    (assoc state :curr-dir next-dir)))

(defn foward
  [state amount]
  (let [{:keys [curr-dir]} state]
    (move-to-dir! state curr-dir amount)))

(defn final-state
  [input]
  (let [directions (parse-input input)]
    (loop [state {:curr-dir :E 
                  :x 0
                  :y 0}
           directions directions]
      (if (not (seq directions))
        state
        (let [[ins arg] (directions 0)]
          (case ins
            :F
            (recur (foward state arg)
                   (into [] (next directions)))

            (:R :L)
            (recur (get-next-dir state ins arg)
                   (into [] (next directions)))

            (:N :E :S :W)
            (recur (move-to-dir! state ins arg)
                   (into [] (next directions)))))))))

(defn manhattan-distance
  [input]
  (->> input
       final-state
       vals
       (filter number?)
       (map #(Math/abs %))
       (apply +)))

(manhattan-distance input)
;; => 1645

;; part 2

(defn move-to-waypoint
  [state amount]
  (let [{:keys [waypoint]} state
        steps (for [dir (keys waypoint)
                    :let [factor (dir waypoint)]]
                [dir (* amount factor)])]
    (loop [state state
           steps steps]
      (if (not (seq steps))
        state
        (let [[dir amount] (first steps)]
          (recur (move-to-dir! state dir amount)
                 (next steps)))))))

(defn rotate-waypoint
  [state dir amount]
  (let [{:keys [waypoint]} state
        new-waypoint (for [card (keys waypoint)
                           :let [pos (card waypoint)
                                 new-dir (-> turns
                                             card
                                             dir
                                             (get amount))]]
                [new-dir pos])]
   (assoc state :waypoint (into {} new-waypoint))))


(defn change-waypoint
  [state dir amount]
  (update-in state [:waypoint dir] + amount))

(defn final-state-2
  [input]
  (let [directions (parse-input input)]
    (loop [state {:x 0
                  :y 0
                  :waypoint {:N 1
                             :E 10
                             :S 0
                             :W 0}}
           directions directions]
      (if (not (seq directions))
        state
        (let [[ins arg] (directions 0)]
          (case ins
            :F
            (recur (move-to-waypoint state arg)
                   (into [] (next directions)))

            (:R :L)
            (recur (rotate-waypoint state ins arg)
                   (into [] (next directions)))

            (:N :E :S :W)
            (recur (change-waypoint state ins arg)
                   (into [] (next directions)))))))))

(defn manhattan-distance-2
  [input]
  (->> input
       final-state-2
       vals
       (filter number?)
       (map #(Math/abs %))
       (apply +)))

(manhattan-distance-2 input)
;; => 35292
