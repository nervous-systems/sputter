(ns sputter.word.arithmetic
  (:refer-clojure :exclude [+ - inc dec *]))

(defmacro << [a b]
  `(bit-shift-left ~a ~b))

(defmacro >>> [a b]
  `(unsigned-bit-shift-right ~a ~b))

(defmacro * [x & xs]
  (if (not-empty xs)
    `(unchecked-multiply ~x (* ~@xs))
     x))

(defmacro + [x & xs]
  (if (not-empty xs)
    `(unchecked-add ~x (+ ~@xs))
    x))

(defmacro - [x & xs]
  (if (not-empty xs)
    `(unchecked-subtract ~x (- ~@xs))
    x))

(defmacro inc [a]
  `(unchecked-inc ~a))

(defmacro dec [a]
  `(unchecked-dec ~a))

(def int-mask* (dec (bit-shift-left 1 (* 4 8))))

(defmacro int-mask [x]
  `(bit-and ~x ~int-mask*))

(defn bit-length [v]
  (loop [i 0
         v v]
    (if (zero? v)
      i
      (recur (inc i) (>>> v 1)))))
