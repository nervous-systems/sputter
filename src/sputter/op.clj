(ns sputter.op
  (:require [pandect.algo.sha3-256 :as sha]
            [sputter.op.table      :as op.table]
            [sputter.word          :as word]
            [sputter.state.memory  :as mem]
            [sputter.util.memory   :as util.mem]
            [sputter.state         :as state]
            [sputter.state.storage :as storage]))

(defmulti operate (fn [state op] (::mnemonic op)))

(defn register-op [mnemonic f]
  (defmethod operate mnemonic [state op]
    (state/push state (apply f (::popped op)))))

(defn register-ops [mnemonic->f]
  (doseq [[mnemonic f] mnemonic->f]
    (register-op mnemonic f)))

(defmethod operate :default [state op]
  (throw (ex-info "Operation not implemented" op)))

(register-ops
 {::add word/add
  ::sub word/sub})

(defmethod operate ::dup [state op]
  (if (< (count (::popped op)) (::variant op))
    (assoc state :sputter/error :stack-underflow)
    (-> (reduce state/push state (::popped op))
        (state/push (last (::popped op))))))

(defmethod operate ::mload [state op]
  (let [[pos]        (::popped op)
        [state word] (mem/recall state pos)]
    (state/push state word)))

(defmethod operate ::mstore [state op]
  (let [remember (case (::variant op)
                   8   util.mem/remember-byte
                   nil mem/remember)]
    (apply remember state (::popped op))))

(defmethod operate ::swap [state op]
  (let [[h & t] (::popped op)
        state   (state/push state h)]
    (reduce state/push state t)))

(defmethod operate ::sstore [state op]
  (apply storage/store state :recipient (::popped op)))

(defmethod operate ::sload [state op]
  (let [[pos] (::popped op)]
    (storage/retrieve state :recipient pos)))

(defmethod operate ::push [state op]
  (state/push state (::data op)))

(defmethod operate ::jumpdest [state op]
  state)

(defmethod operate ::jump [state op]
  (state/position state (first (::popped op))))

(defmethod operate ::jumpi [state op]
  (let [[pos v] (::popped op)]
    (cond-> state (not (word/zero? v)) (state/position pos))))
