(ns sputter.word
  (:require [sputter.util            :as util]
            [sputter.util.biginteger :as b])
  (:import [java.math BigInteger]))

(def size      32)
(def max-value (-> b/one (b/<< (* size 8)) (b/- b/one)))

(defprotocol VMWord
  (truncate [word]
    "Contain within maximum word size.")
  (add [word x]
    "Add two words, returning a word.")
  (sub [word x]
    "Subtract two words, returning a word.")
  (join [word other n]
    "Replace `n` least-significant _bytes_ in `word` with `n` most-significant
     bytes from `other, returning `word`.")
  (insert [word b pos]
    "Insert single-byte word `b` at `pos` in `word`."))

(extend-type BigInteger
  VMWord
  (truncate [word]
    (b/and word max-value))
  (add [word x]
    (-> word (b/+ x) truncate))
  (sub [word x]
    (-> word (b/- x) truncate))
  (join [word other n]
    (b/or (truncate (b/<< word (* 8 n)))
          (b/>> other (* 8 (- size n)))))
  (insert [word b pos]
    (let [prefix (b/<< (b/>> word (* 8 (- size pos))) 8)
          suffix (b/and word (b/mask (* 8 (- size pos 1))))]
      (b/or (b/<< (b/or prefix b)
                  (* 8 (- size pos 1)))
            suffix))))

(def word? (partial satisfies? VMWord))

(defn ->Word [x]
  (cond
    (word?   x) x
    (number? x) (biginteger x)
    (string? x) (BigInteger. 1 (util/hex->bytes x))
    :else       (BigInteger. 1 x)))

(def zero  (->Word 0))
