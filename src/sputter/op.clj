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

(defn- zero-guard [f]
  (fn [x y]
    (if (word/zero? y)
      y
      (f x y))))

(register-op ::add word/add)
(register-op ::sub word/sub)
(register-op ::mul word/mul)
(register-op ::div (zero-guard word/div))
(register-op ::mod (zero-guard word/mod))
(register-op ::or  word/or)
(register-op ::gt (fn [x y] (if (< y x) word/one word/zero)))
(register-op ::lt (fn [x y] (if (< x y) word/one word/zero)))

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

(defmethod operate ::return [state op]
  (let [[state i] (apply util.mem/recall-biginteger state (::popped op))]
    (assoc state :sputter/return i)))

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
    (if (not= (::mnemonic (state/instruction state')) ::jumpdest)
      (assoc state :sputter/error :invalid-jump)
      (assoc state' :sputter/advance? false))))

(defmethod operate ::jump [state op]
  (->> op ::popped first (jump* state)))

(defmethod operate ::jumpi [state op]
  (let [[pos v] (::popped op)]
    (cond-> state (not (word/zero? v)) (jump* pos))))
