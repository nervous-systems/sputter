(ns sputter.gas
  (:require [sputter.gas.yellow :as gas.yellow]
            [sputter.word       :as word]
            [sputter.tx.memory  :as mem]
            [sputter.storage    :as storage]
            [sputter.op         :as op]))

(defmulti ^:private op->variable-cost
  (fn [op constants] (::op/mnemonic op)))

(defmethod op->variable-cost :default [_ _] 0)

(defn- mem-fee [words per-word]
  (-> words
      (* words)
      (quot (* word/size 16))
      (+ (* words per-word))))

(derive ::op/mstore ::mem-extend)
(derive ::op/mload  ::mem-extend)
(derive ::op/return ::mem-extend)

(defn- mem-op->byte-width [op]
  (case (::op/mnemonic op)
    ::op/return (second (::op/popped op))
    ::op/mstore (if-let [variant (::op/variant op)]
                  (/ variant 8)
                  word/size)
    word/size))

(defmethod op->variable-cost ::mem-extend [op constants]
  (let [width      (mem-op->byte-width op)
        [addr]     (::op/popped op)
        prev-words (-> op :sputter/tx mem/words)
        curr-words (int (Math/ceil (/ (+ addr width) word/size)))]
    (max 0 (- (mem-fee curr-words (:per-memory-word constants))
              (mem-fee prev-words (:per-memory-word constants))))))

(defmethod op->variable-cost ::op/sstore [op constants]
  (let [[pos cur] (::op/popped op)
        prev      (-> op :sputter/tx (storage/retrieve :recipient pos))]
    (cond
      (and (word/zero? prev)
           (not (word/zero? cur)))  (:sset constants)
      (and (word/zero? cur)
           (not (word/zero? prev))) (:sreset constants)  ;; fixme refund
      :else                         (:sreset constants))))

(defprotocol GasModel
  (fixed-cost [_ mnemonic]
    "Return the up-front cost of executing the op w/ the given `mnemonic`.")
  (variable-cost [_ op tx]
    "Return the cost of executing `op` against `tx`.

    Does not include any fixed-cost component."))

(defrecord EthGasModel [constants by-op]
  GasModel
  (fixed-cost [_ mnemonic]
    (by-op mnemonic))
  (variable-cost [_ op tx]
    (op->variable-cost
     (assoc op :sputter/tx tx)
     constants)))

(def yellow-paper
  (->EthGasModel gas.yellow/constants gas.yellow/by-op))
