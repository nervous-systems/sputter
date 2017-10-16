(ns sputter.state.memory
  (:require [sputter.word            :as word]
            [sputter.util.biginteger :as b]))

(defprotocol VMMemory
  (store [mem at-byte word]
    "Store` `word` at position `at-byte` within `mem`,
     Returns `mem`.")
  (recall [mem from-byte n-bytes]
    "Read a [[java.math.BigInteger]] `n-bytes` wide from `from-byte`.
     Returns vector of `[mem big-integer]`")
  (stored [mem]
    "Return the number of words in `mem`, or the number of words
     implied by out-of-bounds accesses of `mem`, whichever is greater."))

(defn- trim-biginteger [b pos n]
  (let [extra (rem (+ pos n) word/size)]
    (-> b
        (cond-> (pos? extra)
          (b/>> (* 8 (- word/size extra))))
        (b/and (b/mask (* 8 n))))))

(defn- slot-range [from-byte n-bytes]
  (range (quot from-byte word/size)
         (Math/ceil (/ (+ from-byte n-bytes)
                       word/size))))

(defn- update* [mem slot f & args]
  (apply update-in mem [:table slot] (fnil f word/zero) args))

(defrecord Memory [table extent]
  VMMemory
  (store [mem at-byte word]
    (let [slot    (quot at-byte word/size)
          in-slot (- word/size (rem at-byte word/size))
          mem     (update* mem slot word/join word in-slot)]
      (if (= word/size in-slot)
        (update mem :extent max slot)
        (-> mem
            (update* (inc slot) #(word/join word % in-slot))
            (update :extent max (inc slot))))))

  (recall [mem from-byte n-bytes]
    (let [slots (slot-range from-byte n-bytes)
          b     (reduce
                 (fn [acc slot]
                   (b/or (b/<< acc (* 8 word/size))
                         (word/as-biginteger
                          (table slot word/zero))))
                 b/zero
                 slots)]
      [(update mem :extent max (last slots))
       (trim-biginteger b from-byte n-bytes)]))

  (stored [mem]
    (inc extent)))

(def memory? (partial satisfies? VMMemory))

(defn ->Memory [x & [extent]]
  (cond
    (memory? x) x
    (map?    x) (Memory. (into (sorted-map) x) (or extent -1))))
