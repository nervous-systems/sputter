(ns sputter.word
  (:import [java.math BigInteger])
  (:refer-clojure :exclude [zero?]))

(defprotocol VMWord
  "A collection of retrieval and arithmetic functionality for EVM words."
  (signed [word]
    "A signed BigInteger interpretation of the word.")
  (unsigned [word]
    "An unsigned BigInteger interpretation of the word.")
  (add [word x]
    "Add two words, returning a word.")
  (sub [word x]
    "Subtract two words, returning a word.")
  (zero? [word]))

(defrecord Word [raw]
  VMWord
  (signed [_]
    (biginteger raw))
  (unsigned [_]
    (BigInteger. 1 raw))
  (add [word x]
    (-> (unsigned word) (.add (unsigned x)) .toByteArray Word.))
  (sub [word x]
    (-> (unsigned word) (.subtract (unsigned x)) .toByteArray Word.))
  (zero? [word]
    (= 0 (unsigned word))))

(defn ->Word [x]
  (cond
    (satisfies? VMWord x) x
    (number?           x) (-> x biginteger .toByteArray Word.)
    (seq               x) (Word. x)
    :else (throw (ex-info "Can't coerce to Word" {:value x}))))

(def zero (->Word 0))
