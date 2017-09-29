(ns sputter.state.memory
  (:require [sputter.word            :as word]
            [sputter.util.biginteger :as b]))

(defprotocol VMMemory
  (remember [mem pos word]
    "Store `word` starting at byte position `pos` in `mem`.
     Returns `mem`.")
  (recall [mem pos]
    "Retrieve the word starting at byte position `pos` in `mem`.

     Returns a vector of `[mem word]`.")
  (remembered [mem]
    "Return the number of words in `mem`, or the number of words
     implied by out-of-bounds accesses of `mem`, whichever is greater."))

(defn- update* [mem pos f & args]
  (apply
   update-in mem [:table pos]
   (fnil f word/zero) args))

(defrecord Memory [table extent]
  VMMemory
  (remember [mem pos word]
    (let [slot    (quot pos word/size)
          in-slot (- word/size (rem pos word/size))]
      (-> mem
          (update* slot word/join word in-slot)
          (cond-> (< in-slot word/size)
            (update* (inc slot) #(word/join word % in-slot))))))

  (recall [mem pos]
    (let [slot      (quot pos word/size)
          from-next (rem pos word/size)
          extent'   (cond-> slot (not (zero? from-next)) inc)]
      [(update mem :extent max (inc extent'))
       (word/join
        (table slot word/zero)
        (table (inc slot) word/zero)
        from-next)]))

  (remembered [mem]
    (let [k (some-> table last key)]
      (max extent (inc (or k -1))))))

(def memory? (partial satisfies? VMMemory))

(defn ->Memory [x & [extent]]
  (cond
    (memory? x) x
    (map?    x) (Memory. (into (sorted-map) x) (or extent 0))))
