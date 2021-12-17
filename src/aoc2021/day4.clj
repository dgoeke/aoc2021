(ns aoc2021.day4
  (:require [aoc2021.core :refer [day]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def board-size 5)
(def input
  (let [[numbers & boards] (str/split (day 4) #"\n\n")
        numbers (map read-string (str/split numbers #","))
        boards (->> boards
                  (map #(str/split % #"[\W]"))
                  (map (partial remove str/blank?))
                  (map (partial map read-string)))]
    {:numbers numbers :boards boards}))

(defn rows [board] (partition board-size board))
(defn cols [board] (apply mapv vector (rows board)))

(defn winner? [numbers board]
  (boolean (or (some #(every? numbers %) (rows board))
               (some #(every? numbers %) (cols board)))))

(defn check-games [{:keys [numbers winners remaining]} number]
  (let [new-numbers (conj numbers number)
        {new-winners true new-remaining false} (group-by (partial winner? (set new-numbers)) remaining)]
    {:numbers new-numbers
     :winners (if new-winners (concat winners new-winners) winners)
     :remaining new-remaining}))

(def results (reductions check-games
                         {:numbers [] :winners [] :remaining (:boards input)}
                         (:numbers input)))

(defn score [{:keys [numbers winners]}]
  (let [winning-number (last numbers)
        number-total (reduce + (set/difference (set winners) (set numbers)))]
    (* number-total winning-number)))

(def part1 (-> (first (drop-while #(empty? (:winners %)) results))
              (update :winners first)
              score))  ; => 4662

(def part2 (-> (first (drop-while #(seq (:remaining %)) results))
              (update :winners last)
              score))  ; => 12080
