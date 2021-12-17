(ns aoc2021.day16
  (:require [aoc2021.core :refer [day]]))

(defmulti parse-part              ; mutually recursive with parse-packet
  (fn [type-id [first-bit]]
    (cond
      (= 4 type-id)     :literal
      (zero? first-bit) :operator-by-length
      :else             :operator-by-count)))

(defn binary-split-at [n bin-seq]   ; split-at for a seq of 0/1s, and bin->dec the left half
  (update (split-at n bin-seq) 0 #(Integer/parseInt (apply str %) 2)))

(defn pred->int-fn [pred]
  (fn [& args] (if (apply pred args) 1 0)))

(def versions (atom []))   ; hack alert
(def clj-fns `{0 +
               1 *
               2 min
               3 max
               4 identity
               5 (pred->int-fn >)
               6 (pred->int-fn <)
               7 (pred->int-fn =)})

(defn parse-packet [packet]
  (let [[version rem] (binary-split-at 3 packet)
        [type-id rem] (binary-split-at 3 rem)]
    (swap! versions conj version)
    (let [[subpackets rem] (parse-part type-id rem)]
      [(cons (clj-fns type-id) subpackets) rem])))

(defmethod parse-part :literal [_ packet]
  (loop [rem packet
         num 0]
    (let [[num-part new-rem] (binary-split-at 4 (rest rem))
          new-num (-> num (* 16) (+ num-part))]
      (if (zero? (first rem))
        [[new-num] new-rem]
        (recur new-rem new-num)))))

(defmethod parse-part :operator-by-length [_ packet]
  (let [[len rem] (binary-split-at 15 (rest packet))  ; length is provided, parse that many bits
        [bits rem] (split-at len rem)]
    (loop [bits bits
           subpackets []]
      (if (empty? bits)
        [subpackets rem]
        (let [[subpacket rem] (parse-packet bits)]
          (recur rem (conj subpackets subpacket)))))))

(defmethod parse-part :operator-by-count [_ packet]
  (let [[len rem] (binary-split-at 11 (rest packet))] ; number of subpackets is provided
    (loop [rem rem
           subpackets []
           i 0]
      (if (>= i len)
        [subpackets rem]
        (let [[subpacket rem] (parse-packet rem)]
          (recur rem (conj subpackets subpacket) (inc i)))))))

(def input (map {\0 0 \1 1} (-> (day 16) (BigInteger. 16) (.toString 2))))

(def part2 (eval (first (parse-packet input))))   ; => 13476220616073
(def part1 (reduce + @versions))                  ; => 940
