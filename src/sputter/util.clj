(ns sputter.util
  (:require [clojure.string          :as str]
            [pandect.algo.keccak-256 :as kk-256])
  (:import [java.math        BigInteger]
           [io.nervous.juint UInt256]
           [java.util        Arrays]))

(def sha3       kk-256/keccak-256)
(def sha3-bytes kk-256/keccak-256-bytes)

(def ^:private hex-chars (char-array "0123456789ABCDEF"))

(defn- bytes->hex-str [^bytes bs]
  (-> (areduce bs i sb (StringBuilder. (* 2 (alength bs)))
        (let [v (aget bs i)
              a (bit-and (bit-shift-right v 4) 0xf)
              b (bit-and v 0xf)]
          (-> sb
              (.append (aget ^chars hex-chars a))
              (.append (aget ^chars hex-chars b)))))
      str))

(let [_0 (int \0)
      _9 (int \9)
      A  (int \A)
      F  (int \F)
      a  (int \a)
      f  (int \f)]
  (defn- hex-char->nib [char]
    (let [ord (int char)]
      (cond
        (<= _0 ord _9)    (- ord _0)
        (<=  A ord  F) (+ (- ord  A) 10)
        (<=  a ord  f) (+ (- ord  a) 10)
        :else
        (throw (IllegalArgumentException. "Illegal character."))))))

(defn- hex-str->bytes [^String s]
  (let [len (.length s)]
    (when-not (zero? (mod len 2))
      (throw (IllegalArgumentException. "Odd length string.")))
    (let [out (byte-array (/ len 2))]
      (loop [i 0]
        (if (= i len)
          out
          (let [hi (hex-char->nib (.charAt s i))
                lo (hex-char->nib (.charAt s (inc i)))]
            (aset out (/ i 2) (unchecked-byte (+ (* 0x10 hi) lo)))
            (recur (+ i 2))))))))

(defn hex->bytes [s]
  (-> (str/replace s "0x" "")
      hex-str->bytes))

(defn byte-count [l]
  (int (Math/ceil (/ (- 64 (Long/numberOfLeadingZeros l)) 8))))

(defn long->bytes [l]
  (let [x   (byte-count l)
        out (byte-array x)]
    (loop [x (dec x), l l]
      (if (neg? x)
        out
        (do
          (aset out x (unchecked-byte (bit-and l 0xff)))
          (recur (dec x) (bit-shift-right l 8)))))))

(defn bytes->long
  ([bs]
   (bytes->long bs 0 (alength ^bytes bs)))
  ([bs i n]
   (loop [out 0, i i, n n]
     (if (zero? n)
       out
       (recur (bit-or (bit-shift-left out 8)
                      (bit-and 0xff (aget ^bytes bs i)))
              (inc i)
              (dec n))))))

(defn- print-hex [bs]
  (let [bs (cond (instance? BigInteger bs) (.toByteArray ^BigInteger bs)
                 (instance? UInt256    bs) (.toByteArray ^UInt256    bs)
                 (number? bs)              (.toByteArray (biginteger bs))
                 :else                     bs)]
    (-> (bytes->hex-str bs)
        (str/replace #"^0+" ""))))

(defn- pad [s opts]
  (let [bs     (/ (count s) 2)
        zeroes (* 2 (- (:pad-left opts bs) bs))]
    (str (apply str (repeat zeroes "0")) s)))

(defn bytes->hex [bs & [opts]]
  (let [strings (->> bs print-hex str/lower-case)]
    (pad (apply str strings) opts)))

(defn byte-slice [bytes i len]
  (Arrays/copyOfRange ^bytes bytes (long i) (long (+ i len))))

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

(defn- longest-prefix [a b]
  (take-while identity (map (fn lp [aa bb] (when (= aa bb) aa)) a b)))

(defn split-prefix [a b]
  (let [prefix (longest-prefix a b)
        n      (count prefix)]
    [(not-empty prefix) (drop n a) (drop n b)]))
