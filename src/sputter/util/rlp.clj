(ns sputter.util.rlp
  (:require [sputter.util :refer [byte-count long->bytes bytes->long]])
  (:import [java.io ByteArrayOutputStream]
           [java.util Arrays])
  (:refer-clojure :exclude [vector?]))

(def ^:private cutoff    55)
(def ^:private str-short 0x80)
(def ^:private str-long  (+ str-short cutoff))
(def ^:private seq-short 0xc0)
(def ^:private seq-long  (+ seq-short cutoff))

(def ^:private max-len   0xffffffffffffffff)

(defprotocol RLP
  (stream-encode [this stream opts]))

(defn- compact? [bs]
  (and (= (count bs) 1)
       (< (bit-and (first bs) 0xff) str-short)))

(defn- write-long [stream l]
  (let [bs (long->bytes l)]
   (.write stream bs 0 (count bs))))

(defn- conform-count [n]
  (when (<= max-len n)
    (throw (ex-info "Length exceeded." {:length n})))
  (unchecked-long n))

(defn- write-prefix [stream cnt magic]
  (if (<= cnt cutoff)
    (.write stream (+ cnt magic))
    (do
      (write-long stream (+ (byte-count cnt) magic cutoff))
      (write-long stream cnt))))

(extend-type nil
  RLP
  (stream-encode [_ stream opts]
    (write-prefix stream 0 str-short)))

(extend-type (type (byte-array 0))
  RLP
  (stream-encode [bs stream opts]
    (cond (:raw? opts)  (.write stream bs 0 (conform-count (count bs)))
          (compact? bs) (.write stream bs 0 1)
          :else
          (let [cnt (conform-count (count bs))]
            (write-prefix stream cnt str-short)
            (.write stream bs 0 cnt)))))

(extend-protocol RLP
  String
  (stream-encode [x stream opts]
    (stream-encode (.getBytes x) stream opts))
  clojure.lang.IPersistentVector
  (stream-encode [xs stream opts]
    (let [stream' (ByteArrayOutputStream.)]
      (doseq [x xs]
        (stream-encode x stream' opts))
      (write-prefix stream (.size stream') seq-short)
      (.writeTo stream' stream))))

(defn encode [x & [opts]]
  (let [stream (ByteArrayOutputStream.)]
    (stream-encode x stream opts)
    (.toByteArray stream)))

(defn- read-wide-bounds [bs i width-width]
  (let [width (bytes->long bs (inc i) width-width)]
    [(+ i width-width 1) width]))

(defn- read-bounds [bs i]
  (let [c (bit-and 0xff (aget bs i))]
   (cond (< c str-short)  [i 1]
         (<= c str-long)  [(inc i) (- c str-short)]
         (<  c seq-short) (read-wide-bounds bs i (- c str-long))
         (<= c seq-long)  [(inc i) (- c seq-short)]
         :else            (read-wide-bounds bs i (- c seq-long)))))

(defn- split-vector [bs]
  (let [n (count bs)]
    (loop [i 0, acc []]
      (if (<= n i)
        acc
        (let [[start width] (read-bounds bs i)
              end           (+ i width (- start i))]
          (recur
           end
           (conj acc (Arrays/copyOfRange bs i end))))))))

(defn vector? [bs]
  (<= seq-short (bit-and 0xff (aget bs 0))))

(defn decode [bs]
  (let [[start width] (read-bounds bs 0)]
    (when-not (= width 0)
      (let [out (Arrays/copyOfRange bs start (+ start width))]
        (cond-> out (vector? bs) split-vector)))))
