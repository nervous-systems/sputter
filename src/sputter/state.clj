(ns sputter.state
  "Immutable EVM execution state."
  (:refer-clojure :exclude [pop]))

(defprotocol State
  "EVM execution state"
  (advance [state offset]
    "Increment the instruction pointer by `offset`, returning the state.")
  (position [state pos]
    "Set the instruction pointer to `pos`")
  (instruction [state]
    "Returns the instruction at pointer.")
  (push [state value]
    "Push `value` onto the stack, returning the state.")
  (pop [state num]
    "Pop up to `num` values from the stack, returning `[state values]`"))

(defrecord VMState [code pointer stack]
  State
  (advance [state offset]
    (update state :pointer + offset))
  (position [state pos]
    (assoc state :pointer pos))
  (instruction [state]
    (code pointer))
  (push [state value]
    (update state :stack conj value))
  (pop [state num]
    (let [[h t] (split-at num stack)]
      [(assoc state :stack (apply list t)) h])))

(defn map->VMState
  "Return a state record, optionally initialized with values from `defaults`"
  [& [defaults]]
  (VMState. (:code    defaults {})
            (:pointer defaults 0)
            (:stack   defaults '())))
