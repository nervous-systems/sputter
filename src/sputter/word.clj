(ns sputter.word
  (:require [sputter.util      :as util]
            [sputter.util.uint :as u]
            [clojure.string    :as str])
  (:import [io.nervous.juint UInt256])
  (:refer-clojure :exclude [zero? or mod]))

(def size      32)
(def max-value (u/mask (* 8 size)))

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
  (as-uint [word]
    "Return a [[UInt256]] representation of `word`." ))

(extend-type UInt256
  VMWord
  (add
    ([word x]   (u/+ word x))
    ([word x m] (.addmod word x m)))
  (sub [word x] (u/- word x))
  (mul
    ([word x]   (u/* word x))
    ([word x m] (.mulmod word x m)))
  (div [word x] (u// word x))
  (mod [word x] (u/mod word x))
  (or [word x]
    (u/or word x))
  (zero? [word]
    (u/zero? word))
  (as-vector [word]
    (apply vector-of :byte (u/to-byte-array word size)))
  (as-uint [word]
    word))

(def word? (partial satisfies? VMWord))

(defn ->Word [x]
  (cond
    (word?   x) x
    (number? x) (UInt256. (biginteger x))
    (string? x) (UInt256. (cond-> x
                            (str/starts-with? x "0x") (subs 2))
                          16)
    (coll?   x) (UInt256. (byte-array x))
    :else       (UInt256. x)))

(def one  (->Word 1))
(def zero (->Word 0))
