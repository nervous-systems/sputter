(ns sputter.word
  (:require [sputter.util            :as util]
            [sputter.util.biginteger :as b])
  (:import [java.math BigInteger])
  (:refer-clojure :exclude [zero? or mod]))

(def size      32)
(def max-value (b/mask (biginteger (* 8 size))))

(defprotocol VMWord
  (add
    [word x]
    [word x m]
    "`word` + `x` [% `m`]")
  (sub [word x]
    "`word` - `x`")
  (mul
    [word x]
    [word x m]
    "`word` * `x` [% `m`]")
  (mod [word x]
    "`word` % `x`")
  (div [word x]
    "`word` / `x`")
  (or [word x]
    "`word` | `x`")
  (zero? [word])
  (as-vector [word]
    "Return a fixed length, zero-padded byte vector representation of
     `word`'s underlying bytes.")
  (as-biginteger [word]
    "Return a [[java.math.BigInteger]] representation of `word`'s
     underlying bytes."))

(defn- truncate [word]
  (b/and word max-value))

(extend-type BigInteger
  VMWord
  (add
    ([word x]   (-> word (b/+ x) truncate))
    ([word x m] (-> word (b/+ x) (mod m))))
  (sub [word x]
    (-> word (b/- x) truncate))
  (mul
    ([word x]   (-> word (b/* x) truncate))
    ([word x m] (-> word (b/* x) (mod m))))
  (div [word x]
    (-> word (b// x) truncate))
  (mod [word x]
    (-> word (b/mod x) truncate))
  (or [word x]
    (-> word (b/or x) truncate))
  (zero? [word]
    (clojure.core/zero? word))
  (as-vector [word]
    (apply vector-of :byte (b/to-byte-array word size)))
  (as-biginteger [word]
    word))

(def word? (partial satisfies? VMWord))

(defn ->Word [x]
  (cond
    (word?   x) x
    (number? x) (biginteger x)
    (string? x) (BigInteger. 1 (util/hex->bytes x))
    (coll?   x) (BigInteger. 1 (byte-array x))
    :else       (BigInteger. 1 x)))

(def one  (->Word 1))
(def zero (->Word 0))
