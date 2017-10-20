(ns sputter.word.fixed
  (:refer-clojure :exclude [bit-and zero? mod]))

(defprotocol FixedInteger
  (lshift [x other])
  (rshift [x other])
  (add [x other])
  (sub [x other])
  (mul [x other])
  (bit-and [x other])
  (bit-length [x])
  (bit-count [x])
  (zero? [x])
  (divmod [x other])
  (div    [x other])
  (mod    [x other])
  (->biginteger [x]))
