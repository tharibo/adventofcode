(ns adventofcode.day18)
(def input-txt (clojure.string/replace (slurp "C:/users/tharibo/Documents/dev/adventofcode/day18.input.txt") "\r\n" ""))
(def test-input-txt (clojure.string/replace "
.#.#.#
...##.
#....#
..#...
#.#..#
####.." "\n" ""))

(defn index-to-coords [w i]
  (let [x (rem i w)
        y (quot i w)]
    [x y]))
(defn coords-to-index [w [x y]]
  (if (or (< x 0)
            (< y 0)
            (>= x w)
            (>= y w))
    -1
    (+ x (* y w))))
(coords-to-index 10 (index-to-coords 10 49))

(def coords-to-index-6 (partial coords-to-index 6))
(def index-to-coords-6 (partial index-to-coords 6))
(coords-to-index-6 (index-to-coords-6 49))
(def coords-to-index-100 (partial coords-to-index 100))
(def index-to-coords-100 (partial index-to-coords 100))

(def c2i coords-to-index-100)
(def i2c index-to-coords-100)
(def W 100)

(defn neighbors [[x y :as coords] grid]
  (def neighbors-relative-coordinates [[-1 -1] [0 -1] [1 -1]
                                       [-1 0]         [1 0]
                                       [-1 1]  [0 1]  [1 1]])
  (let [neighbors-coords (map (partial map +) neighbors-relative-coordinates (repeat coords))
        neighbors-values (map #(get grid (c2i %)) neighbors-coords)]
    (apply str neighbors-values)))

(defn next-state
  ([grid] (apply str (map-indexed (partial next-state grid) grid)))
  ([grid i v]
   (let [coords (i2c i)
         the-neighbors (neighbors coords grid)
         nb-on (count (filter #{\#} the-neighbors))
         was-on (= \# v)]
     (cond (= 3 nb-on) \#
           (and was-on (= 2 nb-on)) \#
           :defaut \.))))

(next-state test-input-txt)

(defn result [f input times]
  (if (= 0 times) input
      (recur f
             (f input)
             (dec times))))

;(def total-on (count (filter #{\#} (result next-state input-txt 100))))

(defn real-grid [grid]
  (apply str (apply (partial assoc (apply vector grid)) (flatten (map vector [0 (dec W) (* W (dec W)) (dec (* W W))] (repeat \#))))))

(defn next-state2
  ([grid] (real-grid (apply str (map-indexed (partial next-state (real-grid grid)) (real-grid grid)))))
  ([grid i v]
   (let [coords (i2c i)
         the-neighbors (neighbors coords grid)
         nb-on (count (filter #{\#} the-neighbors))
         was-on (= \# v)]
     (cond (= 3 nb-on) \#
           (and was-on (= 2 nb-on)) \#
           :defaut \.))))

(def total-on2 (count (filter #{\#} (result next-state2 input-txt W))))
