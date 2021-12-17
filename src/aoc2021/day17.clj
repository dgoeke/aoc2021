(ns aoc2021.day17
  (:require [aoc2021.core :refer [day]]))

(def input
  (->> (day 17) first (re-seq #"(-?\d+)") (map first) (map read-string)
       (zipmap [:xmin :xmax :ymin :ymax])))

(defn triangular-number [n] (-> n (* (inc n)) (/ 2)))
(defn inverse-triangular-number [a] (int (Math/sqrt (* a 2))))

(defn simulate-one [{:keys [xmin xmax ymin ymax]} v0_x v0_y]
  (loop [x 0 y 0 vx v0_x vy v0_y]
    (cond
      (or (> x xmax) (< y ymin)) false      ; overshot the target
      (and (>= x xmin) (<= y ymax)) true    ; inside the target area
      :else (recur (+ x vx) (+ y vy) (max (dec vx) 0) (dec vy)))))

(defn simulate-bounded [{:keys [xmin xmax ymin]}]
  (count (filter true? (for [v0_x (range (inverse-triangular-number xmin) (inc xmax))
                             v0_y (range ymin (- 0 ymin))]
                         (simulate-one input v0_x v0_y)))))

(def part1 (triangular-number (:ymin input)))  ; => 8646
(def part2 (simulate-bounded input))           ; => 5945
