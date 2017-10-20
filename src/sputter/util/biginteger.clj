(ns sputter.util.biginteger
  (:import [java.math BigInteger])
  (:refer-clojure :exclude [or and - + * / not mod]))

(def one  BigInteger/ONE)
(def zero (biginteger 0))

(defmacro <<  [x y] `(.shiftLeft  ^BigInteger ~x ~y))
(defmacro >>  [x y] `(.shiftRight ^BigInteger ~x ~y))
(defmacro pow [x y] `(.pow        ^BigInteger ~x ~y))
(defmacro mod [x y] `(.mod        ^BigInteger ~x ~y))

(defmacro not [x] `(.not ^BigInteger ~x))

(defmacro -   [x & xs] (if (not-empty xs) `(.subtract ^BigInteger ~x (-   ~@xs)) x))
(defmacro +   [x & xs] (if (not-empty xs) `(.add      ^BigInteger ~x (+   ~@xs)) x))
(defmacro *   [x & xs] (if (not-empty xs) `(.multiply ^BigInteger ~x (*   ~@xs)) x))
(defmacro /   [x & xs] (if (not-empty xs) `(.divide   ^BigInteger ~x (/   ~@xs)) x))
(defmacro or  [x & xs] (if (not-empty xs) `(.or       ^BigInteger ~x (or  ~@xs)) x))
(defmacro and [x & xs] (if (not-empty xs) `(.and      ^BigInteger ~x (and ~@xs)) x))

(defn ^BigInteger mask
  ([^long m] (-> ^BigInteger one (<< m) (- one)))
  ([^BigInteger n ^long m]
   (and n (mask m))))

(defn to-byte-array [^BigInteger x & [pad-to-bytes]]
  (let [bs       (.toByteArray x)
        n        (int (Math/ceil (clojure.core// (.bitLength x) 8)))
        res      (byte-array (clojure.core/or pad-to-bytes n))
        dest-pos (if pad-to-bytes
                   (clojure.core/- pad-to-bytes n)
                   0)]
    (System/arraycopy bs (clojure.core/- (count bs) n) res dest-pos n)
    res))
