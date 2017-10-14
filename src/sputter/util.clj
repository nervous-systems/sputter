(ns sputter.util
  (:require [clojure.string :as str])
  (:import [javax.xml.bind DatatypeConverter]
           [java.math BigInteger]
           [java.util Arrays]))

(defn hex->bytes [s]
  (DatatypeConverter/parseHexBinary (str/replace s "0x" "")))

(defn- print-hex [bs]
  (let [bs (cond (instance? BigInteger bs) (.toByteArray bs)
                 (number? bs)              (.toByteArray (biginteger bs))
                 :else                     bs)]
    (-> (DatatypeConverter/printHexBinary bs)
        (str/replace #"^0+" ""))))

(defn- pad [s opts]
  (let [bs     (/ (count s) 2)
        zeroes (* 2 (- (:pad-left opts bs) bs))]
    (str (apply str (repeat zeroes "0")) s)))

(defn bytes->hex [bs & [opts]]
  (let [strings (->> bs print-hex str/lower-case)]
    (pad (apply str strings) opts)))

(defn byte-slice [bytes i len]
  (Arrays/copyOfRange bytes (long i) (long (+ i len))))

(defn map-values [f m]
  (into {}
    (for [[k v] m]
      [k (f v)])))

(defmacro for-map
  ([seq-exprs key-expr val-expr]
   `(for-map ~(gensym "m") ~seq-exprs ~key-expr ~val-expr))
  ([m-sym seq-exprs key-expr val-expr]
   `(let [m-atom# (atom (transient {}))]
      (doseq ~seq-exprs
        (let [~m-sym @m-atom#]
          (reset! m-atom# (assoc! ~m-sym ~key-expr ~val-expr))))
      (persistent! @m-atom#))))

(defn error? [v]
  (and (keyword? v) (= (namespace v) "sputter.error")))
