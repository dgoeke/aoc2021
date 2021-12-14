(ns aoc2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn day [^Integer n]
  (->> (format "day%d.txt" n)
       io/resource
       slurp
       str/split-lines))
