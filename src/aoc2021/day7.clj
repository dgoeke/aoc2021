(ns aoc2021.day7
  (:require [aoc2021.core :refer [day]]
            [clojure.string :as str]))

(def input (map read-string (str/split (day 7) #",")))

(def part1 (let [median (nth (sort input) (/ (count input) 2))]
             (reduce + (map #(Math/abs (- % median)) input))))

(def part2 (let [mean (int (/ (reduce + input) (count input)))
                 distance-sum (fn [input target]
                                (reduce + (map #(let [delta (Math/abs (- % target))]
                                                  (-> delta (* (inc delta)) (/ 2)))
                                               input)))]
             (min (distance-sum input mean)
                  (distance-sum input (inc mean)))))
