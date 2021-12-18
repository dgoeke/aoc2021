(ns aoc2021.day6
  (:require [aoc2021.core :refer [day]]
            [clojure.string :as str]))

(def input (map read-string (str/split (day 6) #",")))

(defn evolve [fish]
  (let [new-fish (into {} (map (fn [[age n]] [(dec age) n]) fish))]
    (if-let [spawners (new-fish -1)]
      (-> (dissoc new-fish -1)
         (update 6 (fnil + 0) spawners)
         (update 8 (fnil + 0) spawners))
      new-fish)))

(def part1 (reduce + (vals (nth (iterate evolve (frequencies input)) 80))))
(def part2 (reduce + (vals (nth (iterate evolve (frequencies input)) 256))))
