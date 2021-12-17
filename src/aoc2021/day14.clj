(ns aoc2021.day14
  (:require [aoc2021.core :refer [day-lines]]
            [clojure.string :as str]))

(defn parse-input [data]
  (let [[template _ & rule-text] data
        final-character (last template)
        parsed-input (->> template (partition 2 1) frequencies)
        rules (into {} (->> rule-text
                            (map #(str/split % #" "))
                            (map (fn [[[ch1 ch2] _ [target]]]
                                   {[ch1 ch2] [[ch1 target] [target ch2]]}))))]
    {:input parsed-input
     :rules rules
     :output-state {[final-character] 1}}))

(defn apply-rules-once
  [rules output-state input]
  (->> input
       (mapcat (fn [[pair count]]
                 (map #(hash-map % count) (rules pair))))
       (apply merge-with + output-state)))

(defn iterate-rules
  [iterations {:keys [input rules output-state]}]
  (nth (iterate (partial apply-rules-once rules output-state) input) iterations))

(defn calc-difference [result]
  (->> (reduce-kv (fn [acc [k _] v]
                    (update acc k (fnil + 0) v))
                  {} result)
       vals
       (apply (comp (partial apply -) (juxt max min)))))


(def part1
  (time (->> (parse-input (day-lines 14))
             (iterate-rules 10)
             calc-difference)))           ; => 3.169542 msecs

(def part2
  (time (->> (parse-input (day-lines 14))
             (iterate-rules 40)
             calc-difference)))           ; => 7.592375 msecs
