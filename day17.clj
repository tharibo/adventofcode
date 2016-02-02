(ns adventofcode.day17
  (:require [clojure.math.combinatorics :as combo]))

(def puzzle-input '(33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42))
(def test-puzzle-input '(20 15 10 5 5))

(defn to-object [i x]
  {:index i :value x})

(defn to-value [x]
  (:value x))

(defn result [puzzle-input expected-sum]
  (filter #(= expected-sum %) (map (partial apply +) (map (partial map to-value) (combo/subsets (map-indexed to-object puzzle-input))))))

(result test-puzzle-input 25)
(count (result puzzle-input 150))

(defn combo-object [coll]
  {:count (count coll) :combo coll})

(defn result2 [puzzle-input expected-sum]
  (let [combos (map combo-object (combo/subsets (map-indexed to-object puzzle-input)))
        totals (map #(assoc % :result (apply + (map to-value (:combo %)))) combos)
        solutions (filter #(= expected-sum (:result %)) totals)
        min-count (apply min (map :count solutions))
        solutions2 (filter #(= min-count (:count %)) solutions)]
    solutions2))

(count (result2 puzzle-input 150))
