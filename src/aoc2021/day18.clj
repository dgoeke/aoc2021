(ns aoc2021.day18
  (:require [aoc2021.core :refer [day-lines]]
            [clojure.zip :as zip]))

(def input (map read-string (day-lines 18)))

(defn search [move-fn pred fish]
  (let [fish (move-fn fish)]
    (cond
      (nil? fish)     nil
      (zip/end? fish) nil
      (pred fish)     fish
      :else           (recur move-fn pred fish))))

(def move-next-leaf (partial search zip/next (comp int? zip/node)))
(def move-prev-leaf (partial search zip/prev (comp int? zip/node)))
(def move-to-root (partial search zip/up (comp nil? zip/up)))

(defn explode [fish]
  (let [[left right] (zip/node fish)]
    (as-> fish fish
      (zip/replace fish 0)
      (or (some-> fish move-prev-leaf (zip/edit + left) move-next-leaf) fish)
      (or (some-> fish move-next-leaf (zip/edit + right) move-prev-leaf) fish))))

(defn split [fish]
  (zip/edit fish #(let [val (/ % 2)]
                    [(int (Math/floor val)) (int (Math/ceil val))])))

(defn exploding-node? [n] (and (vector? (zip/node n)) (= 4 (count (zip/path n)))))
(defn splitting-node? [n] (and (int? (zip/node n)) (>= (zip/node n) 10)))

(defn reduce-snailfish [fish]
  (if-let [boom (search zip/next exploding-node? fish)]
    (recur (move-to-root (explode boom)))
    (if-let [splat (search zip/next splitting-node? fish)]
      (recur (move-to-root (split splat)))
      fish)))

(defn magnitude [fish]
  (if (int? fish)
    fish
    (+ (* 3 (magnitude (first fish)))
       (* 2 (magnitude (second fish))))))

(defn add [n1 n2]
  (-> [n1 n2] zip/vector-zip reduce-snailfish zip/root))

(def part1 (magnitude (reduce add input)))       ; => 3734; 490 ms
(def part2 (apply max (for [x input y input      ; => 4837; 7 secs
                            :when (not= x y)]
                        (magnitude (add x y)))))
