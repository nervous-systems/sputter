(ns sputter.state.memory
  (:require [sputter.word            :as word]
            [sputter.util.biginteger :as b]))

(defprotocol VMMemory
  (store [mem slot word]
    "Store `word` at `slot` within `mem`.
     Returns `mem`.")
  (retrieve [mem slot]
    "Retrieve the word at `slot` within `mem`, returning zero if not found.

     Returns a vector of `[mem word]`.")
  (stored [mem]
    "Return the number of words in `mem`, or the number of words
     implied by out-of-bounds accesses of `mem`, whichever is greater."))

(defrecord Memory [table extent]
  VMMemory
  (store [mem slot word]
    (update mem :table assoc slot word))

  (retrieve [mem slot]
    [(update mem :extent max (inc slot))
     (table slot word/zero)])

  (stored [mem]
    (let [k (some-> table last key)]
      (max extent (inc (or k -1))))))

(def memory? (partial satisfies? VMMemory))

(defn ->Memory [x & [extent]]
  (cond
    (memory? x) x
    (map?    x) (Memory. (into (sorted-map) x) (or extent 0))))
