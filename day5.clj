(ns adventofcode.day5)
(use 'clojure.java.io)

(def strings (let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day5.input.txt")]
  (line-seq r)))

(def vowels '(\a \e \i \o \u))

(defn has-vowel [s]
  (boolean (some identity (map #(some (partial = %) s) vowels))))

(defn count-vowels [s]
  (reduce #(+ %1 (second %2)) 0 (select-keys (frequencies s) vowels)))

(defn has-3-vowels? [s]
  (boolean (>= (count-vowels s) 3)))


(defn has-double-letter? [s]
  (:has-double-letter
   (reduce #(hash-map :prev-value %2
                      :has-double-letter (or (:has-double-letter %1)
                                             (= (:prev-value %1) %2)))
           {:prev-value nil :has-double-letter false}
           s)))

(defn contains-ab-cd-pq-xy? [s]
  (or (.contains s "ab")
      (.contains s "cd")
      (.contains s "pq")
      (.contains s "xy")))

(defn is-nice-string? [s]
  (and (has-3-vowels? s)
       (has-double-letter? s)
       (not (contains-ab-cd-pq-xy? s))))

(frequencies (map is-nice-string? strings))

(defn- find-pair-that-repeats
  ([s] (if (< 0 (count s)) (find-pair-that-repeats (apply str (take 2 s)) (apply str (drop 2 s))) nil))
  ([pair s] (if (re-find (re-pattern pair) s)
              pair
              (find-pair-that-repeats (str (second pair) s)))))
(defn- pair-repeats-twice? [s]
  (boolean (find-pair-that-repeats s)))

(defn- find-letter-repeats-with-one-in-between [s]
  (if (> 0 (count s)) nil
      (let [letter (first s)
            r (rest s)
            third-letter (second r)]
        (if (= letter third-letter) letter
            (recur (apply str r))))))

(defn- letter-repeat-with-one-in-between? [s]
  (boolean (find-letter-repeats-with-one-in-between s)))


(defn is-nice-string2? [s]
  (and (pair-repeats-twice? s)
       (letter-repeat-with-one-in-between? s)))

(frequencies (map is-nice-string2? strings))
