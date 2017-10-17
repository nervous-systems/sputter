(ns sputter.state.memory
  (:require [sputter.word            :as word]
            [clojure.core.rrb-vector :as rrb]))

(defprotocol VMMemory
  "Ephemeral byte-addressed data store."
  (insert [mem dest-byte byte-vec n]
    "Copy `n` right-most bytes from `byte-vec` to position `dest-byte` in `mem`.
     Returns `mem`.")
  (recall [mem from-byte n-bytes]
    "Read `n-bytes` from `mem`, starting at `from-byte`, extending if necessary.
     Returns vector of `[mem byte-vec]`")
  (words [mem]
    "Return the number of words in `mem`"))

(defn- extend* [table byte-pos]
  (let [need (- byte-pos (count table))]
    (cond-> table
      (pos? need) (rrb/catvec (into (vector-of :byte)
                                (repeat need 0))))))

(extend-type (type (rrb/vector))
  VMMemory
  (insert [mem dest-byte byte-vec n]
    (let [data (rrb/subvec byte-vec (- (count byte-vec) n))
          mem  (extend* mem (+ dest-byte n))]
      (-> mem
          (rrb/subvec 0    dest-byte)
          (rrb/catvec data (rrb/subvec mem (+ dest-byte n))))))
  (recall [mem from-byte n-bytes]
    (let [mem (extend* mem (+ from-byte n-bytes))]
      [mem (rrb/subvec mem from-byte (+ from-byte n-bytes))]))
  (words [mem]
    (int (Math/ceil (/ (count mem) word/size)))))

(def memory? (partial satisfies? VMMemory))

(defn ->Memory [x]
  (cond
    (memory? x) x
    (coll?   x) (into (rrb/vector-of :byte) x)))
