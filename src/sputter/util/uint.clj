(ns sputter.util.uint
  (:import [io.nervous.juint UInt256])
  (:refer-clojure :exclude [or and - + * / not mod zero?]))

(def one  UInt256/ONE)
(def zero UInt256/ZERO)

(defmacro zero? [x]
  `(.isZero ~x))

(defmacro <<  [x y] `(.shiftLeft  ~x ~y))
(defmacro >>  [x y] `(.shiftRight ~x ~y))
(defmacro pow [x y] `(.pow        ~x ~y))
(defmacro mod [x y] `(.mod        ~x ~y))

(defmacro not [x] `(.not ~x))

(defmacro -   [x & xs] (if (not-empty xs) `(.subtract ~x (-   ~@xs)) x))
(defmacro +   [x & xs] (if (not-empty xs) `(.add      ~x (+   ~@xs)) x))
(defmacro *   [x & xs] (if (not-empty xs) `(.multiply ~x (*   ~@xs)) x))
(defmacro /   [x & xs] (if (not-empty xs) `(.divide   ~x (/   ~@xs)) x))
(defmacro or  [x & xs] (if (not-empty xs) `(.or       ~x (or  ~@xs)) x))
(defmacro and [x & xs] (if (not-empty xs) `(.and      ~x (and ~@xs)) x))

(defn mask
  ([m] (-> one (<< m) .dec))
  ([n m]
   (and n (mask m))))

(defn to-byte-array [x & [pad-to-bytes]]
  (let [bs       (.toByteArray x)
        n        (int (Math/ceil (clojure.core// (.bitLength x) 8)))
        res      (byte-array (clojure.core/or pad-to-bytes n))
        dest-pos (if pad-to-bytes
                   (clojure.core/- pad-to-bytes n)
                   0)]
    (System/arraycopy bs (clojure.core/- (count bs) n) res dest-pos n)
    res))
