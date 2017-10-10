(ns sputter.util.memory
  "Support functionality for [[mem/VMMemory]]."
  (:require [sputter.state.memory    :as mem]
            [sputter.word            :as word]
            [sputter.util.biginteger :as b]))

(defn- inv-offset [v]
  (- word/size (rem v word/size)))

(defn remember-byte [mem pos b]
  (let [slot       (* word/size (quot pos word/size))
        [mem word] (mem/recall mem slot)]
    (->> (rem pos word/size)
         (word/insert word b)
         (mem/remember mem slot))))

(defn- reduce-words [f mem val positions]
  (reduce
   (fn [[mem acc] pos]
     (let [[mem word] (mem/recall mem pos)]
       [mem (f acc word)]))
   [mem val]
   positions))

(defn- word-boundary? [x]
  (zero? (rem x word/size)))

(defn- boundaried-range [start end]
  (range (* word/size (quot start word/size))
         (* word/size (Math/ceil (/ end word/size)))
         word/size))

(defn recall-biginteger [mem pos n]
  ;; we don't want to perform any reads which unnecessarily
  ;; cross word boundaries because it'll screw up the extent
  ;; calculation.
  (let [end     (+ pos n)
        [mem w] (reduce-words
                 (fn [acc word]
                   (b/or (b/<< acc (* 8 word/size)) word))
                 mem
                 word/zero
                 (boundaried-range pos end))]
    [mem (-> w
             (cond-> (not (word-boundary? end))
               (b/>> (* 8 (inv-offset end))))
             (b/and (b/mask (* 8 n))))]))

(defn recall-bytes [mem pos n]
  (-> (recall-biginteger mem pos n)
      (update 1 b/to-byte-array n)))
