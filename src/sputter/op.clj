(ns sputter.op
  (:require [pandect.algo.sha3-256 :as sha]
            [sputter.op.table      :as op.table]
            [sputter.word          :as word]
            [sputter.state         :as state]))

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

(defmethod operate :sputter.op.group/dup [state op]
  (-> (reduce state/push state (::popped op))
      (state/push (last (::popped op)))))

(defmethod operate :sputter.op.group/swap [state op]
  (let [[h & t] (::popped op)
        state   (state/push state h)]
    (reduce state/push state t)))

(defmethod operate :sputter.op.group/push [state op]
  (state/push state (::data op)))

(defmethod operate ::jumpdest [state op]
  state)

(defmethod operate ::jump [state op]
  (state/position state (first (::popped op))))

(defmethod operate ::jumpi [state op]
  (let [[pos v] (::popped op)]
    (cond-> state (not (word/zero? v)) (state/position pos))))
