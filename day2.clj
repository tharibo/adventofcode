(ns adventofcode.day2)
(use 'clojure.java.io)
(def line "1x2x3")

(defn split [line] (clojure.string/split line #"x"))
(split line)
(map #(Integer/parseInt %) (split line))

(defn exact-surface-area [l w h]
  (let [a (* l w)
        b (* l h)
        c (* w h)
        surface (apply + (map (partial * 2) [a b c]))]
    surface))

(defn smallest-side-surface [b]
  (apply * (doall (take 2 b))))

(defn total-surface [[l w h :as box]]
  (+ (exact-surface-area l w h)
     (smallest-side-surface box)))

(defn ribbon-wrap [b]
  (apply + (doall (map (partial * 2) (take 2 b)))))
(ribbon-wrap [2 3 4])

(defn ribbon-bow [b]
  (apply * b))
(ribbon-bow [2 3 4])

(defn total-ribbon [b]
  (+ (ribbon-wrap b)
     (ribbon-bow b)))

(def box [2 3 4])
(smallest-side-surface box)
(apply exact-surface-area box)
(total-surface box)

(defn to-box [s]
  (sort (map #(Integer/parseInt %) (split s))))

(to-box "2x3x4")


(let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day2.input.txt")
      lines (line-seq r)]
  (reduce #(+ %1 (total-surface (to-box %2))) 0 lines))

(let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day2.input.txt")
      lines (line-seq r)]
  (reduce #(+ %1 (total-ribbon (to-box %2))) 0 lines))
