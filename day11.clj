(ns adventofcode.day11)

(def puzzle-input "hxbxwxba")

(defn find-increasing-straight
  ([s] (find-increasing-straight (take 3 s) (drop 3 s)))
  ([straight s] (let [ints (map int straight)]
                      (if (= (+ 2 (first ints)) (inc (second ints)) (last ints))
                        (apply str straight)
                        (if (< (count s) 3)
                          nil
                          (find-increasing-straight (rest s)))))))

(defn has-not-iol? [s]
  (nil? (some #{\i \o \l} s)))

(defn find-two-pairs
  ([s] (if (> (count s) 1) (find-two-pairs (take 2 s) (drop 2 s)) nil))
  ([[a b] s] (if (= a b)
               (if (> (count s) 1)
                 (find-two-pairs [a b] (take 2 s) (drop 2 s))
                 nil)
               (if (> (count s) 0)
                 (find-two-pairs [b (first s)] (rest s))
                 nil)))
  ([first-pair [a b] s] (if (= a b)
                          [first-pair [a b]]
                          (if (> (count s) 0)
                            (find-two-pairs first-pair [b (first s)] (rest s))
                            nil))))

(defn inc-string [s]
  (apply str (reverse ((fn inc-reversed-string [s]
                        (if (empty? s) "a"
                            (let [c (first s)]
                              (if (= c \z) (cons \a (inc-reversed-string (rest s)))
                                  (cons (char (inc (int c))) (rest s))))))
                       (reverse s)))))

(defn is-password-ok? [s]
  (boolean (and (find-increasing-straight s)
                (has-not-iol? s)
                (find-two-pairs s))))

(defn find-next-password [s]
  (let [candidate (inc-string s)]
    (if (is-password-ok? candidate)
      candidate
      (recur candidate))))
