(ns aoc2021.day23
  (:require [aoc2021.core :refer [day-lines]]
            [loom.graph :as graph]
            [loom.alg :as alg]))

(def move-costs {\A 1 \B 10 \C 100 \D 1000})
(def rooms {1  {:exits #{2}}
            2  {:exits #{1 3}}
            3  {:exits #{2 4 20}  :blocks-room? true}
            4  {:exits #{3 5}}
            5  {:exits #{4 6 22}  :blocks-room? true}
            6  {:exits #{5 7}}
            7  {:exits #{6 8 24}  :blocks-room? true}
            8  {:exits #{7 9}}
            9  {:exits #{8 10 26} :blocks-room? true}
            10 {:exits #{9 11}}
            11 {:exits #{10}}
            20 {:exits #{3 21} :goal \A :contains \A :linked-goal 21}
            21 {:exits #{20}   :goal \A :contains \B :linked-goal 20}
            22 {:exits #{5 23} :goal \B :contains \D :linked-goal 23}
            23 {:exits #{22}   :goal \B :contains \C :linked-goal 22}
            24 {:exits #{7 25} :goal \C :contains \B :linked-goal 25}
            25 {:exits #{24}   :goal \C :contains \A :linked-goal 24}
            26 {:exits #{9 27} :goal \D :contains \D :linked-goal 27}
            27 {:exits #{26}   :goal \D :contains \C :linked-goal 26}})

(def graph (reduce (fn [g [room-num {:keys [exits]}]]
                     (->> (interleave (repeat room-num) exits)
                          (partition 2)
                          (reduce graph/add-edges g)))
                   (graph/graph)
                   rooms))

(defn available-rooms [rooms monster]
  (->> rooms
       (filter (every-pred (comp not :blocks-room? second)
                           (comp not :contains second)))
       (filter (fn [[k {:keys [goal]}]]
                 (or (not goal)
                     (= goal monster))))
       (filter (fn [[k {:keys [linked-goal]}]]
                 (or (not linked-goal)
                     (nil? (:contains (rooms linked-goal)))
                     (= monster (:contains (rooms linked-goal))))))
       (map first)))

(defn occupied-rooms [rooms] (filter (comp :contains second) rooms))

(defn every-room-empty? [rooms path]
  (->> path
       (map (comp :contains rooms))
       (every? nil?)))

(defn possible-moves-per-room [rooms graph] ; valid moves grouped by room
  (->> (let [occupied (occupied-rooms rooms)]
         (for [[room-num room] occupied]
           (let [monster (:contains room)
                 possible-targets (available-rooms rooms monster)
                 paths (->> possible-targets
                            (map #(alg/bf-path graph room-num %))
                            (map rest)
                            (filter (partial every-room-empty? rooms)))]
             [room-num paths])))
       (filter (comp seq second))))

(defn possible-moves [rooms graph]           ; all valid moves sorted by lowest cost
  (->> (possible-moves-per-room rooms graph) ; seq of [start-room cost path]
       (mapcat (fn [[room-num path-list]]
                 (map vector (repeat room-num) path-list)))
       (map (fn [[room-num move-list]]
              [room-num
               (* (-> room-num rooms :contains move-costs) (count move-list))
               (last move-list)]))
       (sort-by second)))

(defn move [rooms start-room target-room]
  (let [monster (:contains (rooms start-room))]
    (-> rooms
        (update start-room dissoc :contains)
        (update target-room assoc :contains monster))))

(defn done? [rooms]
  (->> rooms
       (filter (comp :goal second))
       (every? (fn [[_ {:keys [goal contains]}]]
                 (= goal contains)))))

(def cache (atom {}))

(declare find-lowest-cost)

(defn find-lowest-cost-cached [graph rooms total-cost]
  (let [cache-val (@cache rooms)
        [cached-cost cached-result] cache-val]
    (if (and cache-val (<= cached-cost total-cost))
      cached-result
      (let [new-result (find-lowest-cost graph rooms total-cost)]
        (swap! cache assoc rooms [total-cost new-result])
        new-result))))

(defn find-lowest-cost [graph rooms total-cost]
  (loop [rooms rooms
         lowest-cost 999999999
         explorations (possible-moves rooms graph)]
    (if-not (seq explorations)
      lowest-cost
      (let [[[start-room cost target-room] & rem] explorations
            new-rooms (move rooms start-room target-room)
            new-cost (+ total-cost cost)]
        (if (done? new-rooms)
          (recur rooms (min new-cost lowest-cost) rem)
          (recur rooms (find-lowest-cost-cached graph new-rooms new-cost) rem))))))

(def part1 (find-lowest-cost graph rooms 0))
