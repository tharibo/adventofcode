(ns adventofcode.day8
  (:gen-class))
(use 'clojure.java.io)
(require '[clojure.tools.reader])
(def txt-strings (let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day8.input.txt")]
                        (line-seq r)))

(seq (first txt-strings))
(count (first txt-strings))
(count (clojure.tools.reader/read-string (first txt-strings)))

(reduce (fn [acc s] (let [code-chars-num (count s)
                          memory-chars-num (count (clojure.tools.reader/read-string s))]
                      (+ acc (- code-chars-num memory-chars-num)))) 0 txt-strings)

(char-escape-string (first (first txt-strings)))
(first (first txt-strings))
(count (char-escape-string \\))

(defn escape-string [s]
  (apply str (map #(or (char-escape-string %) %) s)))
(escape-string (first txt-strings))
(count (escape-string (first txt-strings)))

(reduce (fn [acc s] (let [code-chars-num (count s)
                          escaped-chars-num (+ 2 (count (escape-string s)))]
                      (+ acc (- escaped-chars-num code-chars-num)))) 0 txt-strings)

