(ns aoc2021.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> "day1.txt" io/resource slurp str/split-lines (map #(Integer/parseInt %))))

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
