(import 'java.security.MessageDigest
        'java.math.BigInteger)


(def secret-key "iwrupvqb")

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(md5 secret-key)
(take 5 (md5 "abcdef609043"))
(defn starts-with-five-zeros? [s]
  (let [five-firsts (take 5 s)]
    (= (repeat 5 \0) five-firsts)))

(defn starts-with-six-zeros? [s]
  (let [six-firsts (take 6 s)]
    (= (repeat 6 \0) six-firsts)))

(defn find-first-num-that-when-combined-with-key-md5-starts-with-five-zeros
  ([] (find-first-num-that-when-combined-with-key-md5-starts-with-five-zeros 0))
  ([n] (let [s (str secret-key n)
            md5-sum (md5 s)]
        (if (starts-with-five-zeros? md5-sum)
          n
          (recur (inc n))))))
(find-first-num-that-when-combined-with-key-md5-starts-with-five-zeros)

(defn find-first-num-that-when-combined-with-key-md5-starts-with-six-zeros
  ([] (find-first-num-that-when-combined-with-key-md5-starts-with-six-zeros 0))
  ([n] (let [s (str secret-key n)
            md5-sum (md5 s)]
        (if (starts-with-six-zeros? md5-sum)
          n
          (recur (inc n))))))
(find-first-num-that-when-combined-with-key-md5-starts-with-six-zeros)
