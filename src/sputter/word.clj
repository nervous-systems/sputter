(ns sputter.word
  (:import [java.math BigInteger])
  (:refer-clojure :exclude [zero?]))

(defprotocol Word
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

(defrecord VMWord [raw]
  Word
  (signed [_]
    (BigInteger. raw))
  (unsigned [_]
    (BigInteger. 1 raw))
  (add [word x]
    (-> (unsigned word) (.add (unsigned x)) .toByteArray VMWord.))
  (sub [word x]
    (-> (unsigned word) (.subtract (unsigned x)) .toByteArray VMWord.))
  (zero? [word]
    (= 0 (unsigned word))))
