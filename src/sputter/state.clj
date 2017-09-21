(ns sputter.state
  "Immutable EVM execution state."
  (:require [sputter.word :as word]
            [sputter.state.memory :as mem])
  (:refer-clojure :exclude [pop]))

(defprotocol VMState
  "EVM execution state"
  (advance [state offset]
    "Increment the instruction pointer by `offset`, returning the state.")
  (position [state pos]
    "Set the instruction pointer to `pos`")

  (instruction [state]
    "Return the instruction at pointer.")
  (push [state value]
    "Push `value` onto the stack, returning the state.")
  (pop [state num]
    "Pop up to `num` values from the stack, returning `[state values]`"))

(defrecord State [program pointer stack memory]
  VMState
  (advance [state offset]
    (update state :pointer word/add (word/->Word offset)))
  (position [state pos]
    (assoc state :pointer (word/->Word pos)))
  (instruction [state]
    (program pointer))
  (push [state value]
    (update state :stack conj value))
  (pop [state num]
    (let [[h t] (split-at num stack)]
      [(assoc state :stack (apply list t)) h])))

(defn map->State
  "Return a state record, optionally initialized with values from `defaults`"
  [& [defaults]]
  (State. (:program defaults {})
          (:pointer defaults word/zero)
          (:stack   defaults '())
          (mem/->Memory (:memory defaults {}))))
