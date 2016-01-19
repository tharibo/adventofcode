(ns adventofcode.day9
  (:require [clojure.math.combinatorics :as combo]))

(use 'clojure.java.io)

(def txt-strings (let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day9.input.txt")]
                        (line-seq r)))

(def groups (map #(rest (re-find #"(\w+) to (\w+) = (\d+)" %)) txt-strings))

(def costs (map #(cons [(first %) (second %)] [(Integer/parseInt (last %))]) groups))

(def sites (set (flatten (map first adventofcode.day9/costs))))

(def paths (combo/permutations sites))

(defn from-to-equals [[from1 to1]  [from2 to2]]
  (or (and (= from1 from2) (= to1 to2))
      (and (= from1 to2) (= from2 to1))))

(defn get-cost [from to costs]
  (some #(when (from-to-equals [from to] (first %)) (second %)) costs))

(defn cost-of-path
  ([path] (cost-of-path 0 path))
  ([acc path] (let [cost (get-cost (first path) (second path) adventofcode.day9/costs)]
                (if (> (count path) 2)
                  (recur (+ acc cost) (rest path))
                  (+ acc cost)))))

(sort (map cost-of-path paths))
(def result (apply min (map cost-of-path paths)))
(def result2 (apply max (map cost-of-path paths)))

result
result2
