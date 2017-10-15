(ns sputter.word
  (:require [sputter.util            :as util]
            [sputter.util.biginteger :as b])
  (:import [java.math BigInteger])
  (:refer-clojure :exclude [zero?]))

(def size      32)
(def max-value (-> b/one (b/<< (* size 8)) (b/- b/one)))

(defprotocol VMWord
  (truncate [word]
    "Contain within maximum word size.")
  (add [word x]
    "Add two words, returning a word.")
  (sub [word x]
    "Subtract two words, returning a word.")
  (mul [word x]
    "Multiply two words, returning a word.")
  (div [word x]
    "Divide two words, returning a word.")
  (join [word other n]
    "Replace `n` right-most _bytes_ in `word` with `n` left-most
     bytes from `other`, returning `word`.")
  (insert [word pos b]
    "Insert single-byte word `b` at `pos` in `word`.
     A `pos` of zero points to the left-most byte.")
  (zero? [word])
  (as-biginteger [word]
    "Return a [[java.math.BigInteger]] representation of this
    word's data."))

(extend-type BigInteger
  VMWord
  (truncate [word]
    (b/and word max-value))
  (add [word x]
    (-> word (b/+ x) truncate))
  (sub [word x]
    (-> word (b/- x) truncate))
  (mul [word x]
    (-> word (b/* x) truncate))
  (div [word x]
    (-> word (b// x) truncate))
  (join [word other n]
    (b/or (truncate (b/<< word (* 8 n)))
          (b/>> other (* 8 (- size n)))))
  (zero? [word]
    (clojure.core/zero? word))
  (insert [word pos b]
    (let [prefix (b/>>   word (* 8 (- size pos 1)))
          suffix (b/mask word (* 8 (- size pos 1)))]
      (-> prefix
          (b/or (b/mask b 0xFF))
          (b/<< (* 8 (- size pos 1)))
          (b/or suffix))))
  (as-biginteger [word]
    word))

(def word? (partial satisfies? VMWord))

(defn ->Word [x]
  (cond
    (word?   x) x
    (number? x) (biginteger x)
    (string? x) (BigInteger. 1 (util/hex->bytes x))
    :else       (BigInteger. 1 x)))

(def zero (->Word 0))
