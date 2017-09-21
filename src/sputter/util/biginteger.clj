(ns sputter.util.biginteger
  (:import [java.math BigInteger])
  (:refer-clojure :exclude [or and - + not]))

(def one BigInteger/ONE)

(defmacro <<  [x y] `(.shiftLeft  ~x ~y))
(defmacro >>  [x y] `(.shiftRight ~x ~y))
(defmacro pow [x y] `(.pow        ~x ~y))

(defmacro not [x] `(.not ~x))

(defmacro -   [x & xs] (if (not-empty xs) `(.subtract ~x (-   ~@xs)) x))
(defmacro +   [x & xs] (if (not-empty xs) `(.add      ~x (+   ~@xs)) x))
(defmacro or  [x & xs] (if (not-empty xs) `(.or       ~x (or  ~@xs)) x))
(defmacro and [x & xs] (if (not-empty xs) `(.and      ~x (and ~@xs)) x))

(defmacro mask [n]
  `(-> ~one (<< ~n) (- ~one)))
