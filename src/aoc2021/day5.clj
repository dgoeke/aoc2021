(ns aoc2021.day5
  (:require [aoc2021.core :refer [day-lines]]))

(def input (map #(->> (re-seq #"(\d+)" %)
                      (map (comp read-string first))
                      (zipmap [:x1 :y1 :x2 :y2]))
                (day-lines 5)))

(defn locations [{:keys [x1 y1 x2 y2]}]
  (if (or (= x1 x2) (= y1 y2))
    (for [x (range (min x1 x2) (inc (max x1 x2)))
          y (range (min y1 y2) (inc (max y1 y2)))]
      {[x y] 1})
    (let [x-inc (if (> x1 x2) -1 1)
          y-inc (if (> y1 y2) -1 1)]
      (loop [x x1 y y1 result '()]
        (if (or (= x (+ x2 x-inc)) (= y (+ y2 y-inc)))
          result
          (recur (+ x x-inc) (+ y y-inc) (conj result {[x y] 1})))))))

(defn count-overlaps [input filter-fn]
  (->> (filter filter-fn input)
     (mapcat locations)
     (apply merge-with +)
     (filter (fn [[_loc val]] (> val 1)))
     count))

(def part1 (count-overlaps input (fn [{:keys [x1 y1 x2 y2]}] (or (= x1 x2) (= y1 y2)))))
(def part2 (count-overlaps input identity))
