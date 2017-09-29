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

(defn- inv-offset [v]
  (- word/size (rem v word/size)))

(defrecord Memory [table extent]
  VMMemory
  (remember [mem pos word]
    (let [slot    (quot pos word/size)
          in-slot (inv-offset pos)]
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

(defn remember-byte [mem pos b]
  (let [slot       (* word/size (quot pos word/size))
        [mem word] (recall mem slot)]
    (->> (rem pos word/size)
         (word/insert word b)
         (remember mem slot))))

(defn- recall-bigintegers [mem positions]
  (reduce
   (fn [[mem acc] pos]
     (let [[mem word] (recall mem pos)]
       [mem (conj acc (word/as-biginteger word))]))
   [mem []]
   positions))

(defn- word-boundary? [x]
  (zero? (rem x word/size)))

(defn- combine-bigintegers [ws]
  (reduce
   (fn [acc w]
     (b/or (b/<< acc (* 8 word/size)) w))
   word/zero
   ws))

(defn recall-biginteger [mem pos n]
  (let [start    (quot pos word/size)
        end      (Math/ceil (/ (+ pos n) word/size))
        [mem ws] (recall-bigintegers
                  mem
                  (range (* word/size start)
                         (* word/size end)
                         word/size))]
    [mem
     (-> ws
         (update 0 b/mask (* 8 (inv-offset pos)))
         combine-bigintegers
         (cond-> (not (word-boundary? (+ pos n)))
           (b/>> (* 8 (inv-offset (+ pos n))))))]))

(defn recall-bytes [mem pos n]
  (-> (recall-biginteger mem pos n)
      (update 1 b/to-byte-array n)))

(def memory? (partial satisfies? VMMemory))

(defn ->Memory [x & [extent]]
  (cond
    (memory? x) x
    (map?    x) (Memory. (into (sorted-map) x) (or extent 0))))
