(ns sputter.vm
  (:require [sputter.op       :as op]
            [sputter.op.table :as op.table]
            [sputter.util     :as util]
            [sputter.word     :as word]
            [sputter.state    :as state]))

(defmulti disassemble-op (fn [op bytes] (::op/mnemonic op)))
(defmethod disassemble-op :default [op bytes] op)

(defmethod disassemble-op :sputter.op.group/push [op bytes]
  (let [size  (inc (- (::op/code op) op.table/push-min))
        start (inc (::pos op))
        n     (util/byte-slice bytes start size)]
    (assoc op
      ::op/width size
      ::op/data  (word/->Word n))))

(defn- read-op [bytes i]
  (let [op (-> (get bytes i) (bit-and 0xFF))]
    (-> op op.table/ops (assoc ::pos i) (disassemble-op bytes))))

(defn disassemble [bytes]
  (let [bytes (cond-> bytes (string? bytes) util/hex->bytes)]
    (loop [i    0
           prog (sorted-map)]
      (if (<= (count bytes) i)
        prog
        (let [op   (read-op bytes i)
              prog (assoc prog i op)
              i    (+ i 1 (::op/width op 0))]
          (recur i prog))))))

(defn step [state]
  (if (::terminated? state)
    state
    (if-let [op (state/instruction state)]
      (let [[state popped] (state/pop state (::op/stack-pop op))]
        (-> state
            (op/operate (assoc op ::op/popped popped))
            (state/advance (+ 1 (::op/width op 0)))))
      (assoc state ::terminated? true))))

(defn execute [state]
  (->> state
       (iterate step)
       (drop-while (complement ::terminated?))
       first))
