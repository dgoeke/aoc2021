(ns aoc2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "day2.txt" io/resource slurp str/split-lines
                (map #(str/split % #" "))
                (map (fn [[dir amt]]
                       [(keyword dir) (Integer/parseInt amt)]))))

(defmulti reduce-p1 (fn [_state [dir _amt]] dir))

(defmethod reduce-p1 :forward [[x y] [_ amt]] [(+ x amt) y])
(defmethod reduce-p1 :up [[x y] [_ amt]] [x (- y amt)])
(defmethod reduce-p1 :down [[x y] [_ amt]] [x (+ y amt)])

(def part1 (apply * (reduce reduce-p1 [0 0] input)))

(defmulti reduce-p2 (fn [_state [dir _amt]] dir))

(defmethod reduce-p2 :forward [[x y aim] [_ amt]] [(+ x amt) (+ y (* aim amt)) aim])
(defmethod reduce-p2 :up [[x y aim] [_ amt]] [x y (- aim amt)])
(defmethod reduce-p2 :down [[x y aim] [_ amt]] [x y (+ aim amt)])

(def part2 (apply * (take 2 (reduce reduce-p2 [0 0 0] input))))
