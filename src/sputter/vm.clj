(ns sputter.vm
  (:require [sputter.op       :as op]
            [sputter.op.table :as op.table]
            [sputter.util     :as util]
            [sputter.word     :as word]
            [sputter.state    :as state]))

(defmulti disassemble-op (fn [op bytes] (::op/mnemonic op)))
(defmethod disassemble-op :default [op bytes] op)

(defmethod disassemble-op :sputter.op.type/push [op bytes]
  (let [size  (inc (- (::op/code op) op.table/push-min))
        start (inc (::pos op))
        n     (util/byte-slice bytes start size)]
    (assoc op ::op/width size ::op/data (word/->VMWord n))))

(defn- read-op [bytes i]
  (let [op (bit-and 0xFF (get bytes i))]
    (-> op op.table/ops (assoc ::pos i) (disassemble-op bytes))))

(defn disassemble [bytes]
  (let [bytes (cond-> bytes (string? bytes) util/hex->bytes)]
    (loop [i    0
           code (sorted-map)]
      (if (<= (count bytes) i)
        code
        (let [op   (read-op bytes i)
              code (assoc code i op)
              i    (+ i 1 (::op/width op 0))]
          (recur i code))))))

(defn step [state]
  (if (::terminated? state)
    state
    (if-let [op (state/instruction state)]
      (let [[state stack] (state/pop state (::op/stack-pop op))]
        (-> state
            (op/operate (assoc op ::op/stack stack))
            (state/advance (+ 1 (::op/width op 0)))))
      (assoc state ::terminated? true))))

(defn execute [state]
  (->> state
       (iterate step)
       (drop-while (complement ::terminated?))
       first))
