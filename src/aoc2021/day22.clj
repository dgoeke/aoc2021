(ns aoc2021.day22
  (:require [aoc2021.core :refer [day-lines]]
            [clojure.string :as str]))

(defn create-instruction [onoff nums]
  (let [[x1 x2 y1 y2 z1 z2]
        (map (comp read-string second) (re-seq #"(-?\d+)" nums))]
    {:on? (= "on" onoff) :min [x1 y1 z1] :max [x2 y2 z2]}))

(defn disjoint? [{c1-max :max c1-min :min} {c2-max :max c2-min :min}]
  (let [smaller-coord? (fn [[x1 y1 z1] [x2 y2 z2]]
                         (or (< x1 x2) (< y1 y2) (< z1 z2)))]
    (or (smaller-coord? c1-max c2-min) (smaller-coord? c2-max c1-min))))

(defn apply-pairwise [f coords1 coords2] (mapv (partial apply f) (map vector coords1 coords2)))

(defn intersection [{:as cube1 c1-max :max c1-min :min} {:as cube2 c2-max :max c2-min :min} on?]
  (when-not (disjoint? cube1 cube2)
    {:on? on?
     :min (apply-pairwise max c1-min c2-min)
     :max (apply-pairwise min c1-max c2-max)}))

(defn add-cubes [cube-list {:as cube :keys [on?]}]
  (let [intersect-fn (fn [cube2] (intersection cube cube2 (not (:on? cube2))))]
    (concat (cond-> cube-list on? (conj cube))
            (keep intersect-fn cube-list))))

(defn total-volume [cubes]
  (let [{on-cubes true off-cubes false} (group-by :on? cubes)
        volume (fn [{:keys [min max]}] (apply * (apply-pairwise (comp inc -) max min)))]
    (- (reduce + (map volume on-cubes))
       (reduce + (map volume off-cubes)))))

(defn init-region-intersection [{:as cube :keys [on?]}]
  (intersection cube {:on? true :min [-50 -50 -50] :max [50 50 50]} on?))

(def input (->> (map #(str/split % #"[ \n]") (day-lines 22))
                (map (partial apply create-instruction))))

(def part1 (->> (keep init-region-intersection input)
                (reduce add-cubes [])
                total-volume))                            ; => 615700, 120 ms

(def part2 (total-volume (reduce add-cubes [] input)))    ; => 1236463892941356, 3.5 secs
