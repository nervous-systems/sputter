(ns sputter.tx
  "Immutable EVM transaction state."
  (:require [sputter.word         :as word]
            [sputter.tx.memory    :as mem]
            [sputter.storage      :as storage]
            [sputter.storage.stub :as storage.stub])
  (:refer-clojure :exclude [pop]))

(defprotocol EVMTransaction
  "EVM transaction execution state."
  (position [state pos]
    "Set the instruction pointer to `pos`")
  (instruction [state]
    "Return the instruction at pointer.")

  (deduct-gas [state n]
    "Deduct `n` from the remaining gas tally, or return a tx with a
     `:sputter/error` of `:gas-insufficient`, if doing so results in a
     negative number.")

  (push [state value]
    "Push `value` onto the stack, returning the tx.")
  (pop [state num]
    "Pop up to `num` values from the stack, returning `[tx values]`"))

(defn- expand-addr [msg addr]
  (case addr
    :recipient (:recipient msg)
    addr))

(defrecord Transaction [program pointer stack memory storage gas message]
  EVMTransaction
  (position [tx pos]
    (assoc tx :pointer (word/->Word pos)))
  (deduct-gas [tx n]
    (if (neg? (- gas n))
      (assoc  tx :sputter/error :gas-insufficient)
      (update tx :gas - n)))
  (instruction [tx]
    (program pointer))
  (push [tx value]
    (update tx :stack conj value))
  (pop [tx num]
    (let [[h t] (split-at num stack)]
      [(assoc tx :stack (apply list t)) h]))

  storage/VMStorage
  (store [tx addr pos word]
    (update tx :storage
            storage/store (expand-addr message addr) pos word))
  (retrieve [tx addr pos]
    (storage/retrieve storage (expand-addr message addr) pos))
  (stored [tx addr]
    (storage/stored storage addr))

  mem/VMMemory
  (insert [tx dest-byte byte-vec n]
    (update tx :memory mem/insert dest-byte byte-vec n))
  (recall [tx from-byte n-bytes]
    (let [[mem word] (mem/recall memory from-byte n-bytes)]
      [(assoc tx :memory mem) word]))
  (words [_]
    (mem/words memory)))

(defn advance
  "Increment the instruction pointer by `offset`, returning the tx."
  [tx & [offset]]
  (let [pos (word/add (:pointer tx) (word/->Word (or offset 1)))]
    (position tx pos)))

(defn map->Transaction
  "Return a transaction record, optionally initialized with values from `defaults`"
  [& [defaults]]
  (let [mem     (mem/->Memory (:memory defaults []))
        storage (as-> (:storage defaults {}) s
                  (cond-> s (map? s) storage.stub/->Storage))]
    (Transaction.
     (:program defaults {})
     (:pointer defaults word/zero)
     (:stack   defaults '())
     mem
     storage
     (:gas     defaults 0)
     (:message defaults {:recipient word/zero}))))
