(ns aoc2021.day20
  (:require [aoc2021.core :refer [day]]
            [clojure.string :as str]))

(def input (let [[algorithm lines] (str/split (day 20) #"\n\n")
                 split-lines (vec (str/split-lines lines))
                 image-size (count (first split-lines))]
             {:algorithm (set (->> (map-indexed vector algorithm)
                                 (filter (comp (partial = \#) second))
                                 (map first)))
              :dots (set (filter identity (for [x (range 0 image-size)
                                                y (range 0 image-size)]
                                            (when (= \# (get-in split-lines [y x]))
                                              [x y]))))}))

(defn bounds [dots]
  (reduce (fn [{:keys [top bottom left right]} [x y]] {:top (min top y)
                                                      :bottom (max bottom y)
                                                      :left (min left x)
                                                      :right (max right x) })
          {:top 0 :bottom 0 :left 0 :right 0} dots))

(defn shift [dots]
  (set (map (fn [[x y]] [(inc x) (inc y)]) dots)))

(defn lens [dots algorithm [x y]]
  (let [window [(dots [(dec x) (dec y)]) (dots [x (dec y)]) (dots [(inc x) (dec y)])
                (dots [(dec x) y])       (dots [x y])       (dots [(inc x) y])
                (dots [(dec x) (inc y)]) (dots [x (inc y)]) (dots [(inc x) (inc y)])]]
    (some? (get algorithm (Long/parseLong (apply str (map #(if (some? %) \1 \0) window)) 2)))))

(defn single-pass [{:as input :keys [dots algorithm]}]
  (let [dots (shift dots)
        {:keys [top left bottom right]} (bounds dots)]
    (assoc input :dots (reduce (fn [new-dots point]
                                 (if (lens dots algorithm point)
                                   (conj new-dots point)
                                   new-dots))
                               #{} (for [x (range (dec top) (+ 1 right))
                                         y (range (dec left) (+ 1 bottom))]
                                     [x y])))))

(def part1 (count (:dots (nth (iterate single-pass input) 2))))   ; => 5361  ; 80 ms
(def part2 (count (:dots (nth (iterate single-pass input) 50))))  ; => 16826 ; 2 secs
