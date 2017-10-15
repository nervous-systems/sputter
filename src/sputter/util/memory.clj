(ns sputter.util.memory
  "Support functionality for [[mem/VMMemory]]."
  (:require [sputter.state.memory    :as mem]
            [sputter.word            :as word]
            [sputter.util.biginteger :as b]))

(defn- update* [mem slot f & args]
  (let [[mem word] (mem/retrieve mem slot)]
    (mem/store mem slot (apply f word args))))

(defn store [mem pos word]
  (let [slot    (quot pos word/size)
        in-slot (- word/size (rem pos word/size))]
    (-> mem
        (update* slot word/join word in-slot)
        (cond-> (< in-slot word/size)
          (update* (inc slot) #(word/join word % in-slot))))))

(defn store-byte [mem pos b]
  (let [slot (quot pos word/size)]
    (update* mem slot word/insert (rem pos word/size) b)))

(defn retrieve [mem pos]
  (let [slot       (quot pos word/size)
        from-next  (rem pos word/size)
        [mem word] (mem/retrieve mem slot)]
    (if (zero? from-next)
      [mem word]
      (let [[mem word'] (mem/retrieve mem (inc slot))]
        [mem (word/join word word' from-next)]))))

(defn- reduce-words [f mem val positions]
  (reduce
   (fn [[mem acc] pos]
     (let [[mem word] (mem/retrieve mem pos)]
       [mem (f acc word)]))
   [mem val]
   positions))

(defn- trim-biginteger [x pos n]
  (let [extra (rem (+ pos n) word/size)]
    (-> x
        (cond-> (pos? extra)
          (b/>> (* 8 (- word/size extra))))
        (b/and (b/mask (* 8 n))))))

(defn retrieve-biginteger [mem pos n]
  (let [slots     (range (quot pos word/size)
                         (Math/ceil (/ (+ pos n) word/size)))
        [mem out] (reduce-words
                   (fn [acc word]
                     (b/or (b/<< acc (* 8 word/size))
                           (word/as-biginteger word)))
                   mem
                   b/zero
                   slots)]
    [mem (trim-biginteger out pos n)]))

(defn retrieve-bytes [mem pos n]
  (-> (retrieve-biginteger mem pos n)
      (update 1 b/to-byte-array n)))
