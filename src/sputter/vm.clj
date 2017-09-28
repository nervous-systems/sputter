(ns sputter.vm
  (:require [sputter.op       :as op]
            [sputter.op.table :as op.table]
            [sputter.util     :as util]
            [sputter.gas      :as gas]
            [sputter.word     :as word]
            [sputter.state    :as state] :reload))

(defmulti disassemble-op (fn [op bytes] (::op/mnemonic op)))
(defmethod disassemble-op :default [op bytes] op)

(defmethod disassemble-op ::op/push [op bytes]
  (let [start (inc (::pos op))
        n     (util/byte-slice bytes start (dec (::op/width op)))]
    (assoc op ::op/data (word/->Word n))))

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
              i    (+ i (::op/width op))]
          (recur i prog))))))

(defn- terminated? [state]
  (or (not (state/instruction state))
      (:sputter/error state)
      (::terminated?  state)))

(defn- operate [state op]
  (let [[state popped] (state/pop state (::op/stack-pop op))
        op             (assoc op ::op/popped popped)
        v-cost         (gas/variable-cost op state)
        state          (state/deduct-gas state v-cost)]
    (if (:sputter/error state)
      state
      (-> state
          (op/operate    op)
          (state/advance (::op/width op))))))

(defn step [state & [{:keys [gas-model]
                      :or   {gas-model gas/yellow-paper}}]]
  (if (terminated? state)
    (assoc state ::terminated? true)
    (let [op      (state/instruction state)
          f-cost  (gas/fixed-cost (::op/mnemonic op))
          state   (state/deduct-gas state f-cost)]
      (if (:sputter/error state)
        state
        (operate state op)))))

(defn execute [state & [opts]]
  (->> state
       (iterate #(step % opts))
       (drop-while (complement terminated?))
       first))
