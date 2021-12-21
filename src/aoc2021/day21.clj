(ns aoc2021.day21
  (:require [aoc2021.core :refer [day]]))

(defn single-round [{:as state :keys [die active-player players]}]
  (let [{:keys [score pos]} (players active-player)
        [dice-results & [die]] (split-at 3 die)
        new-pos (rem (apply + pos dice-results) 10)
        new-score (+ score (inc new-pos))]
    (-> state
       (update-in [:players active-player] assoc :score new-score :pos new-pos)
       (update :rolls (partial + 3))
       (assoc :die die)
       (assoc :active-player (- 1 active-player)))))

(defn game-over? [{[{p1-score :score} {p2-score :score}] :players}]
  (or (>= p1-score 1000) (>= p2-score 1000)))

(defn score [{:keys [active-player players rolls]}]
  (* rolls (get-in players [active-player :score])))

(def input (->> (re-seq #": (\d+)" (day 21)) (map second) (map read-string)))
(def part1 (score (first (drop-while (comp not game-over?)
                                     (iterate single-round {:rolls 0 :die (cycle (range 1 101))
                                                            :active-player 0
                                                            :players [{:score 0 :pos (dec (first input))}
                                                                      {:score 0 :pos (dec (second input))}]})))))

(def quantum-rolls             ; quantum rolls are the same 27 values every round
  (for [d1 (range 1 4) d2 (range 1 4) d3 (range 1 4)]
    (+ d1 d2 d3)))

(def play-part2                ; recursive + memoization = dynamic programming?
  (memoize
    (fn [my-pos my-score other-pos other-score]
      (cond
        (>= my-score 21)    [1 0]  ; base cases first
        (>= other-score 21) [0 1]
        :else (reduce (fn [[my-wins other-wins] roll]
                        (let [new-pos (rem (+ roll my-pos) 10)
                              new-score (+ my-score new-pos 1)
                              [other-quantum-wins my-quantum-wins] (play-part2 other-pos other-score new-pos new-score)]
                          [(+ my-wins my-quantum-wins) (+ other-wins other-quantum-wins)]))
                      [0 0] quantum-rolls)))))

(def part2 (apply max (play-part2 (dec (first input)) 0 (dec (second input)) 0))) ; => 301304993766094, 240ms
