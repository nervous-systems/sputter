(ns sputter.util.biginteger
  (:import [java.math BigInteger])
  (:refer-clojure :exclude [or and - + not]))

(def one  BigInteger/ONE)
(def zero (biginteger 0))

(defmacro <<  [x y] `(.shiftLeft  ~x ~y))
(defmacro >>  [x y] `(.shiftRight ~x ~y))
(defmacro pow [x y] `(.pow        ~x ~y))

(defmacro not [x] `(.not ~x))

(defmacro -   [x & xs] (if (not-empty xs) `(.subtract ~x (-   ~@xs)) x))
(defmacro +   [x & xs] (if (not-empty xs) `(.add      ~x (+   ~@xs)) x))
(defmacro or  [x & xs] (if (not-empty xs) `(.or       ~x (or  ~@xs)) x))
(defmacro and [x & xs] (if (not-empty xs) `(.and      ~x (and ~@xs)) x))

(defmacro mask
  ([m] `(-> ~one (<< ~m) (- ~one)))
  ([n m]
   `(and ~n (mask ~m))))

(defn to-byte-array [x & [pad-to-bytes]]
  (let [bs       (.toByteArray x)
        n        (int (Math/ceil (/ (.bitLength x) 8)))
        res      (byte-array (clojure.core/or pad-to-bytes n))
        dest-pos  (if pad-to-bytes
                    (clojure.core/- pad-to-bytes n)
                    0)]
    (System/arraycopy
     bs (clojure.core/- (count bs) n) res dest-pos n)
    res))
