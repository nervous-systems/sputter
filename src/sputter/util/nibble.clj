(ns sputter.util.nibble)

(def ^:private terminator 16)

(defprotocol ByteString
  (->nibbles [_]))

(extend-type java.lang.String
  ByteString
  (->nibbles [s]
    (reduce
     (fn [acc c]
       (-> acc
         (conj (/   (int c) 16))
         (conj (mod (int c) 16))))
     (vector-of :byte)
     s)))

(extend-type (type (byte-array 0))
  ByteString
  (->nibbles [a]
    (areduce a i acc (vector-of :byte)
             (let [b (bit-and 0xff (aget a i))]
               (-> acc
                 (conj (/   b 16))
                 (conj (mod b 16)))))))

(defn- ->bytes [xs]
  (byte-array
   (for [[q r] (partition 2 xs)]
     (+ (* 16 q) r))))

(defn terminated? [xs]
  (= terminator (last xs)))

(defn terminate [xs]
  (cond-> xs
    (not (terminated? xs)) (concat (list terminator))))

(defn unterminate [xs]
  (cond-> xs
    (terminated? xs) butlast))

(defn pack [xs]
  (let [[xs flags] (if (terminated? xs)
                     [(unterminate xs) 2]
                     [xs               0])
        odd-len    (mod (count xs) 2)
        prefix     (cond-> [(bit-or flags odd-len)]
                     (zero? odd-len) (conj 0))]
    (->bytes (concat prefix xs))))

(defn unpack [s]
  (let [xs    (->nibbles s)
        flags (first xs)]
    (cond-> (subvec xs 1)
      (not (zero? (bit-and flags 2))) terminate
      (not= 1 (bit-and flags 1))      rest)))
