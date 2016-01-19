(ns adventofcode.day5.reddit
  (:require [clojure.string :as string]))
(def vowel?
  (memoize
    (fn [c]
      ((set "aeiou") c))))

(defn three-vowels [row]
  (= 3 (count
         (sequence (comp (filter vowel?)
                         (take 3))
                   row))))

(defn double-letters [row]
  (= true
     (reduce
       (fn [a x]
         (if (= a x)
           (reduced true)
           x))
       row)))

(defn ugly-string [row]
  (or (.contains row "ab")
      (.contains row "cd")
      (.contains row "pq")
      (.contains row "xy")))

(defn reoccuring-pair [row]
  (= true
     (reduce (fn [m [[i a] [j b]]]
               (let [x [a b]
                     last-index (:last-index m)
                     v (inc (m x 0))]
                 (if (= last-index [x i])
                   m
                   (if (>= v 2)
                     (reduced true)
                     (assoc m x v :last-index [x j])))))
             {}
             (partition 2 1 (map-indexed list row)))))

(defn repeating-letter [row]
  (seq
    (filter (fn [[a _ b]] (= a b))
            (partition 3 1 row))))

; (solve-5-1 (slurp "input5.txt"))
(defn solve-5-1 [input]
  (count
    (filter #(and (three-vowels %)
                  (double-letters %)
                  (not (ugly-string %)))
            (string/split-lines input))))

(defn solve-5-2 [input]
  ;(count
   (filter #(and
             ;(repeating-letter %)
             (reoccuring-pair %)
             )
           (string/split-lines input)))
;)
