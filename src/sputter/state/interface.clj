(ns sputter.state.interface)

(defprotocol State
  "EVM execution state"
  (-advance [state offset]
    "Increment the instruction pointer by `offset`, returning the state.")
  (-position [state pos]
    "Set the instruction pointer to `pos`")
  (instruction [state]
    "Returns the instruction at pointer.")
  (-push [state value]
    "Push `value` onto the stack, returning the state.")
  (-pop [state num]
    "Pop up to `num` values from the stack."))
