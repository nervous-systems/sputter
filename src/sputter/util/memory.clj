(ns sputter.util.memory
  "Support functionality for [[mem/VMMemory]]."
  (:require [sputter.state.memory :as mem]
            [sputter.word         :as word]))

(defn insert-byte [mem pos w]
  (let [b (-> w word/as-uint .byteValue)]
    (mem/insert mem pos (vector-of :byte b) 1)))

(defn insert-word [mem pos w]
  (mem/insert mem pos (word/as-vector w) word/size))

(defn recall-word [mem pos]
  (let [[mem w] (mem/recall mem pos word/size)]
    [mem (word/->Word w)]))
