(ns sputter.util
  (:import [javax.xml.bind DatatypeConverter]
           [java.util Arrays]))

(defn hex->bytes [s]
  (DatatypeConverter/parseHexBinary s))

(defn byte-slice [bytes i len]
  (Arrays/copyOfRange bytes i (+ i len)))
