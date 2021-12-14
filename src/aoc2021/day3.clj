(ns aoc2021.day3
  (:require [aoc2021.core :refer [day]]))

(defn ones-complement [s]
  (apply str (map {\0 \1, \1 \0} s)))

(defn swap-map [x]   ; convert {:a 1 :b 1} to {:b 1 :a 1} so min-key works right
  (->> (vec x)
       (sort-by first)
       reverse
       (into {})))

(defn common-bits [swap-fn key-fn input]
  (->> (apply mapv vector input)           ; transpose the matrix
       (map frequencies)
       (map swap-fn)
       (map (comp key (partial apply key-fn val)))  ; find key with largest (or smallest) value
       (apply str)))

(def most-common-bits (partial common-bits identity max-key))
(def least-common-bits (partial common-bits swap-map min-key))

(defn mult-binary-strings [str1 str2]
  (* (Integer/parseInt str1 2) (Integer/parseInt str2 2)))

(def part1
  (mult-binary-strings (most-common-bits (day 3))
                       (ones-complement (most-common-bits (day 3)))))

(defn rating-finder [data filter-fn]
  (loop [bit-number 0
         data data]
    (let [common-bits (filter-fn data)
          new-data (filter #(= (get % bit-number) (get common-bits bit-number)) data)]
      (if (nil? (second new-data))
        (first new-data)
        (recur (inc bit-number) new-data)))))

(def part2
  (mult-binary-strings (rating-finder (day 3) most-common-bits)
                       (rating-finder (day 3) least-common-bits)))
