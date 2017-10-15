(ns sputter.op.table
  (:require [sputter.util :refer [map-values]]))

(defn- op-variants [lo hi op-name & [ks]]
  (into (sorted-map)
    (for [i (range (- hi lo))
          :let [mnemonic (keyword "sputter.op" (name op-name))]]
      [(+ lo i) (merge {:sputter.op/variant   (inc i)
                        :sputter.op/mnemonic  mnemonic
                        :sputter.op/stack-pop (inc i)
                        :sputter.op/width     1} ks)])))

(defn- ->simple-op [mnemonic stack-pop & [ks]]
  (merge {:sputter.op/mnemonic  mnemonic
          :sputter.op/stack-pop stack-pop
          :sputter.op/width     1} ks))

(def ^:private stack->ops
  {0 {0x5b :sputter.op/jumpdest
      0x00 :sputter.op/stop}

   1 {0x20 :sputter.op/sha3
      0x35 :sputter.op/calldataload
      0x50 :sputter.op/pop
      0x51 :sputter.op/mload
      0x56 :sputter.op/jump}

   2 {0x01 :sputter.op/add    0x02 :sputter.op/mul    0x03 :sputter.op/sub
      0x04 :sputter.op/div    0x05 :sputter.op/sdiv   0x06 :sputter.op/mod
      0x07 :sputter.op/smod   0x0a :sputter.op/exp    0x0b :sputter.op/signextend
      0x10 :sputter.op/lt     0x11 :sputter.op/gt     0x12 :sputter.op/slt
      0x13 :sputter.op/sgt    0x14 :sputter.op/eq     0x16 :sputter.op/and
      0x17 :sputter.op/or     0x18 :sputter.op/xor    0x57 :sputter.op/jumpi
      0x1a :sputter.op/byte   0x52 :sputter.op/mstore 0xf3 :sputter.op/return
      0x55 :sputter.op/sstore}

   3 {0x08 :sputter.op/addmod
      0x09 :sputter.op/mulmod}})

(def simple-ops
  (apply
   merge
   (for [[stack-pop ops] stack->ops]
     (map-values #(->simple-op % stack-pop) ops))))

(def pushes
  (->> (op-variants 0x60 0x80 :push {:sputter.op/stack-pop 0})
       (map-values
        (fn [op]
          (assoc op ::sputter.op/width (inc (::sputter.op/variant op)))))))

(def dups (op-variants 0x80 0x90 :dup))

(def swaps
  (->> (op-variants 0x90 0xa0 :swap)
       (map-values #(update % ::sputter.op/stack-pop inc))))

(def ops
  (merge
   pushes dups swaps simple-ops
   {0x53 (->simple-op :sputter.op/mstore 2 {:sputter.op/variant 8})}))

;; (def ops
;;   {0x30 :sputter.op/address      0x31 :sputter.op/balance      0x32 :sputter.op/origin
;;    0x33 :sputter.op/caller       0x34 :sputter.op/callvalue
;;    0x36 :sputter.op/calldatasize 0x37 :sputter.op/calldatacopy 0x38 :sputter.op/codesize
;;    0x39 :sputter.op/codecopy     0x3a :sputter.op/gasprice

;;    0x40 :sputter.op/prevhash 0x41 :sputter.op/coinbase   0x42 :sputter.op/timestamp
;;    0x43 :sputter.op/number   0x44 :sputter.op/difficulty 0x45 :sputter.op/gaslimit

;;    0x50 :sputter.op/pop         0x52 :sputter.op/swap  0x53 :sputter.op/mload
;;    0x54 :sputter.op/mstore 0x55 :sputter.op/mstore8 0x56 :sputter.op/sload 0x57 :sputter.op/sstore
;;    0x58 :sputter.op/jump   0x59 :sputter.op/jumpi   0x5a :sputter.op/pc    0x5b :sputter.op/msize
;;    0x5c :sputter.op/gas

;;    0xf0 :sputter.op/create 0xf1 :sputter.op/call 0xf2 :sputter.op/return 0xff :sputter.op/suicide})
