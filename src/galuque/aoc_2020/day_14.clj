(ns galuque.aoc-2020.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
  (slurp (io/resource "day_14/sample_input.txt")))

(def input
  (slurp (io/resource "day_14/input.txt")))

(defn parse-mask [mask bit]
  (->> (str/escape mask {\X bit})
       (str "2r")
       read-string))

(defn parse-input [input]
  (let [re-str #(re-find #"(\w+)(\[(\d+)\])? = (.*)" %)
        in (->> input
                str/split-lines
                (map re-str))]
    (for [[_ op _ addr val] in]
      (case op
        "mask" [:mask 
                (parse-mask val 0)
                (parse-mask val 1)]
        
        "mem" [:mem 
               (read-string addr)
               (read-string val)]))))

(def sample-parsed (parse-input sample-input))
(def input-parsed (parse-input input))

(defn apply-mask [num zero one]
  (-> num
      (bit-or zero)
      (bit-and one)))

(defn part1 [input]
  (-> (reduce (fn [{:keys [mask0 mask1] :as mem} [op v1 v2]]
                (case op
                  :mask
                  (assoc mem :mask0 v1 :mask1 v2)
                  
                  :mem
                  (assoc mem v1 (apply-mask v2 mask0 mask1))))
              {}
              input)
      (dissoc :mask0 :mask1)
      vals
      (->> (reduce +))))

(part1 input-parsed)
;; => 6559449933360


;; part 2