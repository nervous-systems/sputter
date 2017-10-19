(ns sputter.word.int256
  (:require [sputter.util            :as util]
            [sputter.util.biginteger :as b] :reload))

(defprotocol FixedInteger
  (lshift [x other])
  (add [x other])
  (sub [x other])
  (rshift [x other])
  (divmod [x other])
  (->biginteger [x]))

(defmacro unchecked-inc [x] `(unchecked-add ~x 1))
(defmacro unchecked-dec [x] `(unchecked-subtract ~x 1))

(defmacro lt? [x y]
  `(neg? (Long/compareUnsigned ~x ~y)))

(defmacro gt? [x y]
  `(pos? (Long/compareUnsigned ~x ~y)))

(declare zero)
(declare one)

(defmacro <<word
  ([currw nextw places]
   `(-> (bit-shift-left ~currw ~places)
        (unchecked-add (unsigned-bit-shift-right ~nextw (- 64 ~places)))))
  ([currw places]
   `(bit-shift-left ~currw ~places)))

(defmacro >>word
  ([currw nextw places]
   `(-> (unsigned-bit-shift-right ~currw ~places)
        (unchecked-add (bit-shift-left ~nextw (- 64 ~places)))))
  ([currw places]
   `(unsigned-bit-shift-right ~currw ~places)))

(defrecord Int256 [a b c d]
  java.lang.Comparable
  (compareTo [_ y]
    (let [v (Long/compareUnsigned a (:a y))]
      (if (zero? v)
        (let [v (Long/compareUnsigned b (:b y))]
          (if (zero? v)
            (let [v (Long/compareUnsigned c (:c y))]
              (if (zero? v)
                (Long/compareUnsigned d (:d y))
                v))
            v))
        v)))
  (lshift [x ps]
    (cond
      (<= ps   0) x
      (>= ps 256) zero

      (=  ps  64) (Int256. b c d 0)
      (=  ps 128) (Int256. c d 0 0)
      (=  ps 192) (Int256. d 0 0 0)

      (<  ps  64) (Int256. (<<word a b ps) (<<word b c ps)
                           (<<word c d ps) (<<word d   ps))
      (<  ps 128) (Int256. (<<word b c ps) (<<word c d ps)  (<<word d ps) 0)
      (<  ps 192) (Int256. (<<word c d ps) (<<word d   ps)  0             0)
      (<  ps 256) (Int256. (<<word d   ps)              0   0             0)))
  (rshift [x ps]
    (cond
      (<= ps   0) x
      (>= ps 256) zero

      (=  ps  64) (Int256. 0 a b c)
      (=  ps 128) (Int256. 0 0 a b)
      (=  ps 192) (Int256. 0 0 0 a)

      (<  ps  64) (Int256.       (>>word a   ps) (>>word b a ps)
                                 (>>word c b ps) (>>word d c ps))
      (<  ps 128) (Int256. 0     (>>word a   ps) (>>word b a ps)  (>>word c b ps))
      (<  ps 192) (Int256. 0 0   (>>word a   ps) (>>word b a ps))
      (<  ps 256) (Int256. 0 0 0 (>>word a   ps))))
  FixedInteger
  (add [_ y]
    (let [a' (unchecked-add ^long a ^long (:a y))
          b' (unchecked-add ^long b ^long (:b y))
          c' (unchecked-add ^long c ^long (:c y))
          d' (unchecked-add ^long d ^long (:d y))]
      (Int256. (cond-> a' (lt? b' b) unchecked-inc)
               (cond-> b' (lt? c' c) unchecked-inc)
               (cond-> c' (lt? d' d) unchecked-inc)
               d')))
  (sub [_ y]
    (let [a' (unchecked-subtract ^long a ^long (:a y))
          b' (unchecked-subtract ^long b ^long (:b y))
          c' (unchecked-subtract ^long c ^long (:c y))
          d' (unchecked-subtract ^long d ^long (:d y))]
      (Int256. (cond-> a' (gt? b' b) unchecked-dec)
               (cond-> b' (gt? c' c) unchecked-dec)
               (cond-> c' (gt? d' d) unchecked-dec)
               d')))
  (divmod [x y])
  (->biginteger [_]
    (-> (biginteger (Long/toUnsignedString a))
        (b/<< 64) (b/or (biginteger (Long/toUnsignedString b)))
        (b/<< 64) (b/or (biginteger (Long/toUnsignedString c)))
        (b/<< 64) (b/or (biginteger (Long/toUnsignedString d))))))

(def zero (Int256. 0 0 0 0))
(def one  (Int256. 0 0 0 1))

(defn ->Int256 [b]
  (let [limit (b/mask (* 8 32))]
    (Int256. (.longValue (b/mask (b/>> b (* 24 8)) limit))
             (.longValue (b/mask (b/>> b (* 16 8)) limit))
             (.longValue (b/mask (b/>> b (* 8  8)) limit))
             (.longValue (b/mask b                 limit)))))
