(ns sputter.state.memory
  (:require [sputter.word            :as word]
            [sputter.util.biginteger :as b]))

(defprotocol VMMemory
  (store [mem pos word]
    "Store `word` starting at byte position `pos` in `mem`.
     Returns `mem`.")
  (store-byte [mem pos b]
    "Store a single byte word `b` at position `pos` in `mem`.
     Returns `mem`.")
  (load-word [mem pos]
    "Retrieve the word starting at byte position `pos` in `mem`.

     Returns a vector of `[mem word]`.")
  (load-biginteger [mem pos n])
  (load-bytes [mem pos n])
  (words [mem]
    "Return the number of words in `mem`."))

(defn- update* [mem pos f & args]
  (apply
   update-in mem [:table pos]
   (fnil f word/zero) args))

(defn- inv-offset [v]
  (- word/size (rem v word/size)))

(defrecord Memory [table extent]
  VMMemory
  (store [mem pos word]
    (let [slot    (quot pos word/size)
          in-slot (inv-offset pos)]
      (-> mem
          (update* slot word/join word in-slot)
          (cond-> (< in-slot word/size)
            (update* (inc slot) #(word/join word % in-slot))))))

  (store-byte [mem pos b]
    (let [slot (quot pos word/size)]
      (update* mem slot word/insert b (rem pos word/size))))

  (load-word [mem pos]
    (let [slot      (quot pos word/size)
          from-next (rem pos word/size)
          extent'   (cond-> slot (not (zero? from-next)) inc)]
      [(update mem :extent max extent')
       (word/join
        (table slot word/zero)
        (table (inc slot) word/zero)
        from-next)]))

  (load-biginteger [mem pos n]
    (let [start    (quot pos word/size)
          end      (quot (+ pos n) word/size)
          [w & ws] (for [i (range start (inc end))]
                     (word/as-biginteger (table i word/zero)))]
      [(update mem :extent max end)
       (-> (reduce
            (fn [acc word]
              (b/or (b/<< acc (* 8 word/size)) word))
            (b/mask w (* 8 (inv-offset pos)))
            ws)
           (b/>> (* 8 (inv-offset (+ pos n)))))]))

  (load-bytes [mem pos n]
    (-> (load-biginteger mem pos n)
        (update 1 b/to-byte-array n)))

  (words [mem]
    (-> table last key (max extent) inc)))

(def memory? (partial satisfies? VMMemory))

(defn ->Memory [x & [extent]]
  (cond
    (memory? x) x
    (map?    x) (Memory. (into (sorted-map) x) (or extent 0))))
