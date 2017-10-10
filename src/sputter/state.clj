(ns sputter.state
  "Immutable EVM execution state."
  (:require [sputter.word          :as word]
            [sputter.state.memory  :as mem]
            [sputter.state.storage :as storage])
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
      (assoc  state :sputter/error ::gas-insufficient)
      (update state :gas - n)))
  (instruction [state]
    (program pointer))
  (push [state value]
    (update state :stack conj value))
  (pop [state num]
    (let [[h t] (split-at num stack)]
      [(assoc state :stack (apply list t)) h]))

  storage/VMStorage
  (retrieve [state addr pos]
    (storage/retrieve storage (expand-addr message addr) pos))
  (store [state addr pos word]
    (update state :storage
            storage/store (expand-addr message addr) pos word))

  mem/VMMemory
  (remember [state pos word]
    (update state :memory mem/remember pos word))
  (recall [state pos]
    (let [[mem word] (mem/recall memory pos)]
      [(assoc state :memory mem) word]))
  (remembered [_]
    (mem/remembered memory)))

(defn advance
  "Increment the instruction pointer by `offset`, returning the state."
  [state offset]
  (position state (word/add (:position state) (word/->Word offset))))

(defn map->State
  "Return a state record, optionally initialized with values from `defaults`"
  [& [defaults]]
  (let [mem     (mem/->Memory (:memory defaults {}))
        storage (storage/->mock-Storage (:storage defaults {}))]
    (State. (:program defaults {})
            (:pointer defaults word/zero)
            (:stack   defaults '())
            mem
            storage
            (:gas     defaults 0)
            (:message defaults {:recipient word/zero}))))
