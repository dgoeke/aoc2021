(ns aoc2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(def day
  (memoize
    (fn [^Integer n]
      (->> (format "day%d.txt" n)
           io/resource
           slurp
           str/split-lines))))
