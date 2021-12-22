(ns aoc2021.day22
  (:require [aoc2021.core :refer [day-lines]]
            [clojure.string :as str]))

(defn create-instruction [onoff nums]
  (let [[x1 x2 y1 y2 z1 z2]
        (map (comp read-string second) (re-seq #"(-?\d+)" nums))]
    [(if (= "on" onoff) :on :off) [x1 x2] [y1 y2] [z1 z2]]))

(defn in-bounds? [& args]
  (let [result (every? #(<= -50 % 50) args)]
    (prn args result)
    result))

(defn update-cuboids [result [onoff [x1 x2] [y1 y2] [z1 z2]]]
  (if-not (in-bounds? x1 x2 y1 y2 z1 z2)
    result
    (let [f (if (= :on onoff) conj disj)]
      (apply f result (for [x (range x1 (inc x2)) y (range y1 (inc y2)) z (range z1 (inc z2))] [x y z])))))

(def part1 (->> (day-lines 22)
                (map #(str/split % #"[ \n]"))
                (map (partial apply create-instruction))
                (reduce update-cuboids #{})
                count))
