(ns sputter.gas
  (:require [sputter.gas.yellow   :as gas.yellow]
            [sputter.word         :as word]
            [sputter.state.memory :as mem]
            [sputter.op           :as op]))

(defmulti ^:private op->variable-cost
  (fn [op constants] (::op/mnemonic op)))

(defmethod op->variable-cost :default [_ _] 0)

(defn- mem-fee [words per-word]
  (-> words
      (* words)
      (quot (* word/size 16))
      (+ (* words per-word))))

(defmethod op->variable-cost ::op/mstore [op constants]
  (let [width      (::op/variant op word/size)
        [addr]     (::op/popped  op)
        prev-words (-> op :sputter/state  :memory mem/words)
        curr-words (Math/ceil (/ (+ addr width) word/size))]
    (max 0 (- (mem-fee curr-words (:per-memory-word constants))
              (mem-fee prev-words (:per-memory-word constants))))))

(defprotocol GasModel
  (fixed-cost [mnemonic]
    "Return the up-front cost of executing the op w/ the given `mnemonic`.")
  (variable-cost [op state]
    "Return the cost of executing `op` against `state`.

    Does not include any fixed-cost component."))

(defrecord EthGasModel [constants by-op]
  GasModel
  (fixed-cost [mnemonic]
    (by-op mnemonic))
  (variable-cost [op state]
    (op->variable-cost
     (assoc op :sputter/state state)
     constants)))

(def yellow-paper
  (->EthGasModel gas.yellow/constants gas.yellow/by-op))
