(ns sputter.word.int128
  "128-bit integer implemented as a record of longs."
  (:refer-clojure :exclude [+ - * inc dec])
  (:require [sputter.util            :as util]
            [sputter.util.biginteger :as b]
            [sputter.word.fixed      :as f]
            [sputter.word.arithmetic :as a
             :refer [+ - * inc dec << >>> int-mask]] :reload))

(defmacro lt? [x y]
  `(neg? (Long/compareUnsigned ~x ~y)))

(defmacro gt? [x y]
  `(pos? (Long/compareUnsigned ~x ~y)))

(declare zero)
(declare one)

(defrecord Int128 [^long a ^long b]
  java.lang.Comparable
  (compareTo [_ y]
    (let [v (Long/compareUnsigned a (:a y))]
      (if (clojure.core/zero? v)
        (Long/compareUnsigned b (:b y))
        v)))
  f/FixedInteger
  (zero? [x]
    (and (zero? b) (zero? a)))
  (bit-and [_ y]
    (Int128. (bit-and a (:a y)) (bit-and b (:b y))))
  (lshift [x ps]
    (cond
      (<= ps   0) x
      (>= ps 128) zero
      (=  ps  64) (Int128. b 0)
      (<  ps  64) (Int128. (+ (<< a ps) (>>> b (- 64 ps))) (<< b ps))
      (<  ps 128) (Int128. (<< b (- ps 64)) 0)))
  (rshift [x ps]
    (cond
      (<= ps   0) x
      (>= ps 128) zero
      (=  ps  64) (Int128. 0 a)
      (<  ps  64) (Int128. (>>> a   ps) (+ (<< a (- 64 ps)) (>>> b ps)))
      (<  ps 128) (Int128. 0 (>>> a (- ps 64)))))
  (mul [_ y]
    (let [x-a-l (>>> a 32)       x-a-r (int-mask a)
          x-b-l (>>> b 32)       x-b-r (int-mask b)

          y-a-l (>>> (:a y) 32)  y-a-r (int-mask (:a y))
          y-b-l (>>> (:b y) 32)  y-b-r (int-mask (:b y))

          p-0-3 (* x-b-r y-b-r)  p-1-3 (* x-b-l y-b-r)  p-2-3 (* x-a-r y-b-r)
          p-3-3 (* x-a-l y-b-r)  p-0-2 (* x-b-r y-b-l)  p-1-2 (* x-b-l y-b-l)
          p-2-2 (* x-a-r y-b-l)  p-3-2 (* x-a-l y-b-l)  p-0-1 (* x-b-r y-a-r)
          p-1-1 (* x-b-l y-a-r)  p-2-1 (* x-a-r y-a-r)  p-3-1 (* x-a-l y-a-r)

          p-0-0 (* x-b-r y-a-l)  p-1-0 (* x-b-l y-a-l)  p-2-0 (* x-a-r y-a-l)
          p-3-0 (* x-a-l y-a-l)

          i4    (int-mask p-0-3)

          i3    (+ (int-mask p-0-2) (>>> p-0-3 32)
                   (int-mask p-1-3) (>>> i4 32))

          i2    (+ (int-mask p-0-1) (>>> p-0-2 32)
                   (int-mask p-1-2) (>>> p-1-3 32)
                   (int-mask p-2-3) (>>> i3    32))

          i1    (+ (int-mask p-0-0) (>>> p-0-1 32)
                   (int-mask p-1-1) (>>> p-1-2 32)
                   (int-mask p-2-2) (>>> p-2-3 32)
                   (int-mask p-3-3) (>>> i2    32))]

      (Int128. (bit-or (<< (int-mask i1) 32) (int-mask i2))
               (bit-or (<< (int-mask i3) 32) (int-mask i4)))))
  (add [_ y]
    (let [a' (+ a ^long (:a y))
          b' (+ b ^long (:b y))]
      (Int128. (cond-> a' (lt? b' b) inc) b')))
  (sub [_ y]
    (let [a' (- a ^long (:a y))
          b' (- b ^long (:b y))]
      (Int128. (cond-> a' (gt? b' b) dec) b')))
  (bit-count [_]
    (+ (Long/bitCount a) (Long/bitCount b)))
  (bit-length [_]
    (if (zero? a)
      (a/bit-length b)
      (+ 64 (a/bit-length a))))
  (div [x y]
    (first (f/divmod x y)))
  (mod [x y]
    (second (f/divmod x y)))
  (divmod [x y]
    (cond
      (f/zero? y)                           [nil   nil]
      (zero? (compare y one))               [x    zero]
      (zero? (compare x y))                 [one  zero]
      (or (f/zero? x) (neg? (compare x y))) [zero    x]
      :else
      (loop [quot* zero
             rem*  zero
             i     (f/bit-length x)]
        (if (zero? i)
          [quot* rem*]
          (let [quot* (f/lshift quot* 1)
                rem*  (cond-> (f/lshift rem* 1)
                        (not (f/zero? (f/bit-and (f/rshift x (dec i)) one))) (f/add one))]
            (if (<= 0 (compare rem* y))
              (recur (f/add quot* one) (f/sub rem* y) (dec i))
              (recur quot* rem* (dec i))))))))
  (->biginteger [_]
    (-> (biginteger (Long/toUnsignedString a))
        (b/<< 64) (b/or (biginteger (Long/toUnsignedString b))))))

(def zero (Int128. 0 0))
(def one  (Int128. 0 1))

(defn biginteger->int128 [^BigInteger b]
  (let [limit (b/mask (* 8 32))]
    (Int128. (.longValue (b/and (b/>> b (* 8  8)) limit))
             (.longValue (b/and b                 limit)))))

(defn ->int128
  ([  b] (Int128. 0 b))
  ([a b] (Int128. a b)))
