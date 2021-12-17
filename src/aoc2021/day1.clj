(ns aoc2021.day1
  (:require [aoc2021.core :refer [day-lines]]))

(def input
  (map #(Integer/parseInt %) (day-lines 1)))

(def part1
  (->> input
       (partition 2 1)
       (filter (partial apply <))
       count))

(def part2
  (->> input
       (partition 3 1)
       (map (partial reduce +))
       (partition 2 1)
       (filter (partial apply <))
       count))
