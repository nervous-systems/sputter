(ns sputter.vm
  (:require [sputter.op       :as op]
            [sputter.op.table :as op.table]
            [sputter.util     :as util]
            [sputter.gas      :as gas]
            [sputter.word     :as word]
            [sputter.util.tx  :refer [tx->]]
            [sputter.tx       :as tx]))

(defmulti disassemble-op (fn [op bytes] (::op/mnemonic op)))
(defmethod disassemble-op :default [op bytes] op)

(defmethod disassemble-op ::op/push [op bytes]
  (let [start (inc (::pos op))
        n     (util/byte-slice bytes start (dec (::op/width op)))]
    (assoc op ::op/data (word/->Word n))))

(defn- read-op [bytes i]
  (let [code (bit-and 0xFF (get bytes i))
        op   (op.table/ops code)]
    (when-not (::op/mnemonic op)
      (throw (ex-info
              (str "Unable to dissassemble: " (util/bytes->hex code))
              {:i i :op op})))
    (-> op (assoc ::pos i) (disassemble-op bytes))))

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

(defn- terminated? [tx]
  (or (not (tx/instruction tx))
      (:sputter/error  tx)
      (:sputter/return tx)
      (::terminated?   tx)))

(defn- operate [tx op gas-model]
  (let [pop-n       (::op/stack-pop op)
        [tx popped] (tx/pop tx pop-n)]
    (if (< (count popped) pop-n)
      (assoc tx :sputter/error :stack-underflow)
      (let [op     (assoc op ::op/popped popped)
            v-cost (gas/variable-cost gas-model op tx)
            tx     (tx-> tx
                         (tx/deduct-gas v-cost)
                         (op/operate       op))]
        (cond-> (dissoc tx :sputter/advance?)
          (:sputter/advance? tx true) (tx/advance (::op/width op)))))))

(defn step
  [tx & [{:keys [gas-model]
          :or   {gas-model gas/yellow-paper}}]]
  (if (terminated? tx)
    (assoc tx ::terminated? true)
    (let [op     (tx/instruction tx)
          f-cost (gas/fixed-cost gas-model (::op/mnemonic op))]
      (tx-> tx
            (tx/deduct-gas f-cost)
            (operate       op gas-model)))))

(defn execute [tx & [opts]]
  (->> tx
       (iterate #(step % opts))
       (drop-while (complement terminated?))
       first))
