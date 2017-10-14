(ns sputter.op
  (:require [pandect.algo.sha3-256 :as sha]
            [sputter.op.table      :as op.table]
            [sputter.word          :as word]
            [sputter.state.memory  :as mem]
            [sputter.util.memory   :as util.mem]
            [sputter.state         :as state]
            [sputter.state.storage :as storage]))

(defmulti operate (fn [state op] (::mnemonic op)))

(defmethod operate :default [state op]
  (assoc state :sputter/error :not-implemented))

(derive ::jumpdest ::no-op)
(derive ::pop      ::no-op)

(defmethod operate ::no-op [state op]
  state)

(defn register-op [mnemonic f]
  (defmethod operate mnemonic [state op]
    (state/push state (apply f (::popped op)))))

(register-op ::add word/add)
(register-op ::sub word/sub)

(defmethod operate ::dup [state op]
  (-> (reduce state/push state (::popped op))
      (state/push (last (::popped op)))))

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
        t       (reverse t)]
    (as-> state s
      (state/push s h)
      (reduce state/push s (rest t))
      (state/push s (first t)))))

(defmethod operate ::sstore [state op]
  (apply storage/store state :recipient (::popped op)))

(defmethod operate ::sload [state op]
  (let [[pos] (::popped op)]
    (storage/retrieve state :recipient pos)))

(defmethod operate ::push [state op]
  (state/push state (::data op)))

(defn- jump* [state target]
  (let [state' (state/position state target)]
    (if (not= (::mnemonic (state/instruction state')) :jumpdest)
      (assoc state :sputter/error :invalid-jump)
      state')))

(defmethod operate ::jump [state op]
  (->> op ::popped first (jump* state)))

(defmethod operate ::jumpi [state op]
  (let [[pos v] (::popped op)]
    (cond-> state (not (word/zero? v)) (jump* pos))))
