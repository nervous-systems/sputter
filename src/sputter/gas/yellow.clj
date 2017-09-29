(ns sputter.gas.yellow
  "Yellow Paper gas prices by operation."
  (:require [sputter.op       :as op]
            [sputter.op.table :as op.table]))

(def constants
  {:zero               0 :base         2 :verylow     3 :low           5
   :mid                8 :high        10 :ext        20 :sload        50
   :jumpdest           1 :sset      2000 :sreset   5000 :sclear    15000
   :suicide        24000 :create   32000 :call       40 :callvalue  9000
   :callnewaccount 25000 :exp         10 :expbyte    10 :log         375
   :logdata            8 :logtopic  375 :sha3        30 :sha3word      6
   :copy               3

   :per-memory-word    3})

(def ^:private by-price
  {:zero     #{::op/stop ::op/suicide ::op/return ::op/sstore}
   :jumpdest #{::op/jumpdest}
   :base
   #{::op/address    ::op/origin   ::op/caller   ::op/callvalue ::op/calldatasize
     ::op/codesize   ::op/gasprice ::op/coinbase ::op/timestamp ::op/number
     ::op/difficulty ::op/gaslimit ::op/pop      ::op/pc        ::op/msize
     ::op/gas}
   :verylow
   #{::op/add   ::op/sub    ::op/not  ::op/lt   ::op/gt  ::op/slt  ::op/sgt
     ::op/eq    ::op/iszero ::op/and  ::op/or   ::op/xor ::op/byte ::op/calldataload
     ::op/mload ::op/mstore ::op/push ::op/dup  ::op/swap}
   :low   #{::op/mul ::op/div ::op/sdiv ::op/mod ::op/smod ::op/signextend}
   :mid   #{::op/addmod ::op/mulmod ::op/jump}
   :high  #{::op/jumpi}
   :ext   #{::op/balance ::op/extcodesize :op/blockhash}
   :sload #{::op/sload}})

(def ^{:doc "Map of symbolic operation names to fixed prices"} by-op
  (apply merge (for [[k vs] by-price] (zipmap vs (repeat (constants k))))))
