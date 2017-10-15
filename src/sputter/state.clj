(ns sputter.state
  "Immutable EVM execution state."
  (:require [sputter.word               :as word]
            [sputter.state.memory       :as mem]
            [sputter.state.storage      :as storage]
            [sputter.state.storage.stub :as storage.stub])
  (:refer-clojure :exclude [pop]))

(defprotocol VMState
  "EVM execution state."
  (position [state pos]
    "Set the instruction pointer to `pos`")
  (instruction [state]
    "Return the instruction at pointer.")

  (deduct-gas [state n]
    "Deduct `n` from the remaining gas tally, or return a state with a
     `:sputter/error` of `:gas-insufficient`, if doing so results in a
     negative number.")

  (push [state value]
    "Push `value` onto the stack, returning the state.")
  (pop [state num]
    "Pop up to `num` values from the stack, returning `[state values]`"))

(defn- expand-addr [msg addr]
  (case addr
    :recipient (:recipient msg)
    addr))

(defrecord State [program pointer stack memory storage gas message]
  VMState
  (position [state pos]
    (assoc state :pointer (word/->Word pos)))
  (deduct-gas [state n]
    (if (neg? (- gas n))
      (assoc  state :sputter/error :gas-insufficient)
      (update state :gas - n)))
  (instruction [state]
    (program pointer))
  (push [state value]
    (update state :stack conj value))
  (pop [state num]
    (let [[h t] (split-at num stack)]
      [(assoc state :stack (apply list t)) h]))

  storage/VMStorage
  (store [state addr pos word]
    (update state :storage
            storage/store (expand-addr message addr) pos word))
  (retrieve [state addr pos]
    (storage/retrieve storage (expand-addr message addr) pos))
  (stored [state addr]
    (storage/stored storage addr))

  mem/VMMemory
  (store [state slot word]
    (update state :memory mem/store slot word))
  (retrieve [state slot]
    (let [[mem word] (mem/retrieve memory slot)]
      [(assoc state :memory mem) word]))
  (stored [_]
    (mem/stored memory)))

(defn advance
  "Increment the instruction pointer by `offset`, returning the state."
  [state & [offset]]
  (position state (word/add (:pointer state) (word/->Word (or offset 1)))))

(defn map->State
  "Return a state record, optionally initialized with values from `defaults`"
  [& [defaults]]
  (let [mem     (mem/->Memory (:memory defaults {}))
        storage (as-> (:storage defaults {}) s
                  (cond-> s (map? s) storage.stub/->Storage))]
    (State. (:program defaults {})
            (:pointer defaults word/zero)
            (:stack   defaults '())
            mem
            storage
            (:gas     defaults 0)
            (:message defaults {:recipient word/zero}))))
