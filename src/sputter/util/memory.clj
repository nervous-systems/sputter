(ns sputter.util.memory
  "Support functionality for [[mem/VMMemory]]."
  (:require [sputter.state.memory    :as mem]
            [sputter.word            :as word]
            [sputter.util.biginteger :as b]))

(defn store-byte [mem pos b]
  (let [slot    (quot pos word/size)
        [mem w] (mem/recall mem slot word/size)
        new-w   (word/insert (word/->Word w) (rem pos word/size) b)]
    (mem/store mem slot new-w)))

(defn recall [mem pos]
  (let [[mem w] (mem/recall mem pos word/size)]
    [mem (word/->Word w)]))

(defn recall-bytes [mem pos n]
  (-> (mem/recall mem pos n)
      (update 1 b/to-byte-array n)))
