(use 'clojure.java.io)
(defn walk-left [[x y]]
  [(dec x) y])
(defn walk-right [[x y]]
  [(inc x) y])
(defn walk-up [[x y]]
  [x (dec y)])
(defn walk-down [[x y]]
  [x (inc y)])

(defn- walk [path step]
  (conj (seq path) (let [last-pos (first path)]
                      (cond (= step \<) (walk-left last-pos)
                            (= step \>) (walk-right last-pos)
                            (= step \^) (walk-up last-pos)
                            (= step \v) (walk-down last-pos)
                            :default last-pos))))

(walk [[0 0]] \v)

(reduce walk [[0 0]] '(\^ \^ \< \<))

(defn char-seq
   [^java.io.Reader rdr]
   (let [chr (.read rdr)]
     (if (> chr 0)
     (cons (char chr) (lazy-seq (char-seq rdr))))))

(let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day3.input.txt")]
 (count (set (reduce walk [[0 0]] (char-seq r)))))

(def santa-path (let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day3.input.txt")]
 (set (reduce walk [[0 0]] (keep-indexed #(if (even? %1) %2) (char-seq r))))))

(def robo-santa-path (let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day3.input.txt")]
 (set (reduce walk [[0 0]] (keep-indexed #(if (odd? %1) %2) (char-seq r))))))

(count (clojure.set/union santa-path robo-santa-path))
