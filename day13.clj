(ns adventofcode.day13
  (:require [clojure.math.combinatorics :as combo]))

(use 'clojure.java.io)

(def input-txt (slurp "c:/users/tharibo/Documents/dev/adventofcode/day13.input.txt"))
;(def input-txt "Alice would gain 54 happiness units by sitting next to Bob.
;Alice would lose 79 happiness units by sitting next to Carol.
;Alice would lose 2 happiness units by sitting next to David.
;Bob would gain 83 happiness units by sitting next to Alice.
;Bob would lose 7 happiness units by sitting next to Carol.
;Bob would lose 63 happiness units by sitting next to David.
;Carol would lose 62 happiness units by sitting next to Alice.
;Carol would gain 60 happiness units by sitting next to Bob.
;Carol would gain 55 happiness units by sitting next to David.
;David would gain 46 happiness units by sitting next to Alice.
;David would lose 7 happiness units by sitting next to Bob.
;David would gain 41 happiness units by sitting next to Carol.")

(def regexp #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.")
(def data (reduce (fn [coll item] (let [p1 (get item 1)
                                                p2 (get item 4)
                                                l-or-g (get item 2)
                                                value (Integer/parseInt (get item 3))]
                                            (cons {:p1 p1
                                                   :p2 p2
                                                   :happiness (if (= "lose" l-or-g) (- value) value)} coll)))
                          [] (re-seq regexp input-txt)))
(def people (seq (set (map :p1 data))))
(def possibilities (combo/permutations people))
(def data2 (concat data
                   (reduce #(concat %1 [{:p1 "me" :p2 %2 :happiness 0}
                                        {:p1 %2 :p2 "me" :happiness 0}])
                           [] people)))
(def people2 (cons "me" people))
(def possibilities2 (combo/permutations people2))

(defn partial-happiness [p1 p2 data]
  (reduce + (map :happiness (filter #(or (and (= p1 (:p1 %)) (= p2 (:p2 %)))
                                                     (and (= p1 (:p2 %)) (= p2 (:p1 %)))) data))))
(defn happiness
  ([coll data]
   (let [items (cons (last coll) coll)]
     (happiness 0 items data)))
  ([acc coll data]
   (if (< (count coll) 2)
     acc
     (let [partial (+ acc (partial-happiness (first coll) (second coll) data))]
       (recur partial (rest coll) data)))))

(def results
  (reduce #(let [h (happiness %2 data)]
             (cons {:happiness h :solution %2} %1))
          [] possibilities))
(def sorted-results (sort-by :happiness results))

(def results2
  (reduce #(let [h (happiness %2 data2)]
             (cons {:happiness h :solution %2} %1))
          [] possibilities2))
(def sorted-results2 (sort-by :happiness results2))
