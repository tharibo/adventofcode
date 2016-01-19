(ns adventofcode.day7)
(use 'clojure.java.io)

(defn to-unsigned-short [n]
  (bit-and n 0xffff))

(def NOT #(to-unsigned-short (bit-not %)))
(def OR #(to-unsigned-short (bit-or %1 %2)))
(def AND #(to-unsigned-short (bit-and %1 %2)))
(def LSHIFT #(to-unsigned-short (bit-shift-left %1 %2)))
(def RSHIFT #(to-unsigned-short (bit-shift-right %1 %2)))

(defn clean-re-results [coll]
  (apply vector (filter #(if (nil? %) false (not (.contains % " "))) coll)))

(def regex #"((\w*)\s)?((\w*)\s)?(\w+) -> \b(\w+)")

(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))

(defn symbol-or-number [value]
  (if (numeric? value) value (symbol value)))

(defn to-instruction [s]
  (let [[t1 t2 t3 :as tokens] (clean-re-results (re-find (re-matcher regex s)))
        assigned (last tokens)]
    (cond (= (count tokens) 2) (list 'def (symbol assigned) (read-string t1))
          (= (count tokens) 3) (list 'def (symbol assigned) (list (read-string t1) (read-string t2)))
          (= (count tokens) 4) (list 'def (symbol assigned) (list (read-string t2)
                                                            (read-string t1)
                                                            (read-string t3))))))

(def txt-instructions (let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day7.input2.txt")]
                        (line-seq r)))

(def instructions (map to-instruction txt-instructions))

(defn operation? [token]
  (or (= token (symbol 'NOT))
      (= token (symbol 'OR))
      (= token (symbol 'AND))
      (= token (symbol 'LSHIFT))
      (= token (symbol 'RSHIFT))))

(defn find-dependencies [instruction]
    (let [rhl (last instruction)]
      (if (list? rhl)
        (let [first-token (first rhl)
              is-operation (operation? first-token)
              is-symbol (symbol? first-token)]
          (if is-operation
            (drop 1 rhl)
            (if is-symbol
              '(first-token)
              '())))
        (if (symbol? rhl)
          (list rhl)
          '()))))

(defn assigned-symbol [instruction]
  (second instruction))

(def dependencies (reduce #(assoc %1 (keyword (str (assigned-symbol %2))) (find-dependencies %2)) {} instructions))
(def waiting (reduce #(assoc %1 (keyword (str (assigned-symbol %2))) %2) {} instructions))

(filter #(= 0 (count (find-dependencies %))) instructions)

(defn all-solved? [deps solved]
  (let [filtered-deps (filter (complement number?) deps)
        keys (map (comp keyword str) filtered-deps)]
    (and (seq? deps)
         (= (count keys) (count (select-keys solved keys))))))

(defn solve [key waiting]
  (println (str "Solving " key))
  (let [instruction (get waiting key)
        value (var-get (eval instruction))]
    (println (str "Solving " instruction " -> " value))
    value))

(defn try-solve [key waiting solved dependencies]
  (let [deps (get dependencies key)]
    ;(println (str "Trying " key ", (first deps): " (first deps)))
    (if (all-solved? deps solved)
      (let [value (solve key waiting)]
        [(dissoc waiting key) (assoc solved key value)])
      [waiting solved])))

(defn resolve-all [waiting solved dependencies]
  (if (= (count waiting) 0)
    solved
    (let [[new-waiting new-solved] (reduce (clojure.core/fn [[w s] [key instr]]
                                             (let [key (keyword (str (assigned-symbol instr)))]
                                               ;(println (str "Trying " instr))
                                               (try-solve key w s dependencies)))
                                           [waiting solved]
                                           waiting)]
      ;(println (str (count new-waiting) " " (count new-solved)))
      (recur new-waiting new-solved dependencies))))

(def solved (resolve-all waiting {} dependencies))
(:a solved)
