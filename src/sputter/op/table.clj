(ns sputter.op.table)

(defn- op-range [lo hi op-name]
  (into (sorted-map)
    (for [i (range (- hi lo))
          :let [k (keyword "sputter.op" (str (name op-name) (inc i)))]]
      [(+ lo i) k])))

(def push-min 0x60)
(def dup-min  0x80)
(def swap-min 0x90)

(def ^:private push  (op-range push-min dup-min  :push))
(def ^:private dup   (op-range dup-min  swap-min :dup))
(def ^:private swap  (op-range swap-min 0xa0     :swap))
(def ^:private stack
  {0x01 :sputter.op/add  0x02 :sputter.op/mul  0x03 :sputter.op/sub
   0x04 :sputter.op/div  0x05 :sputter.op/sdiv 0x06 :sputter.op/mod
   0x07 :sputter.op/smod 0x08 :sputter.op/exp  0x09 :sputter.op/neg
   0x0a :sputter.op/lt   0x0b :sputter.op/gt   0x0c :sputter.op/slt
   0x0d :sputter.op/sgt  0x0e :sputter.op/eq   0x0f :sputter.op/not
   0x10 :sputter.op/and  0x11 :sputter.op/or   0x12 :sputter.op/xor
   0x13 :sputter.op/byte 0x20 :sputter.op/sha3 0x35 :sputter.op/calldataload})

(def ^:private by-kind
  {:sputter.op.group/stack stack
   :sputter.op.group/push  push
   :sputter.op.group/dup   dup
   :sputter.op.group/swap  swap
   :sputter.op.group/jump  {0x56 :sputter.op/jump 0x57 :sputter.op/jumpi}
   :sputter.op.group/misc  {0x5b :sputter.op/jumpdest}})

(doseq [[parent kids] by-kind]
  (doseq [kid (vals kids)]
    (derive kid parent)))

(def mnemonic->signature
  (merge
   (zipmap (vals stack) (repeat [2 1]))
   (zipmap (vals push)  (repeat [0 1]))
   (zipmap (vals dup)   (map vector (rest (range)) (rest (rest (range)))))
   (zipmap (vals swap)  (map vector (rest (rest (range))) (rest (rest (range)))))
   {:sputter.op/not      [1 1]
    :sputter.op/jump     [1 0]
    :sputter.op/jumpi    [2 0]
    :sputter.op/jumpdest [0 0]}))

(def ops
  (into {}
    (mapcat
     (fn [[kind ops]]
       (for [[code op] ops :let [[in out] (mnemonic->signature op)]]
         [code {:sputter.op/mnemonic   op
                :sputter.op/code       code
                :sputter.op/stack-pop  in
                :sputter.op/stack-push out}])))
    by-kind))

(def by-mnemonic
  (into {}
    (for [[code op] ops]
      [(:sputter.op/mnemonic op) op])))

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
