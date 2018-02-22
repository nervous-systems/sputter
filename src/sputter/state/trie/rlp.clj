(ns sputter.state.trie.rlp
  (:require [sputter.state.trie.node :as node]
            [sputter.util.rlp        :as rlp]
            [sputter.util.nibble     :as nibble]
            [sputter.util
             :refer [for-map]])
  (:import [sputter.state.trie.node Branch Leaf Extension]))

(defn vector->node [v]
  (if-not (vector? v)
    v
    (if (= 17 (count v))
      (node/->Branch
       (for-map [[i x] (map-indexed vector v) :when x]
         i x)
       (rlp/decode (peek v)))
      (let [k (nibble/unpack (rlp/decode (first v)))]
        (if (nibble/terminated? k)
          (node/->Leaf      (nibble/unterminate k) (rlp/decode (peek v)))
          (node/->Extension k                      (peek v)))))))

(defprotocol FlattenableNode
  (->rlp [node]))

(extend-protocol FlattenableNode
  Branch
  (->rlp [node]
    (-> (mapv (:branches node) (range 16))
        (conj (rlp/encode (:value node)))
        (rlp/encode {:raw? true})))
  Leaf
  (->rlp [node]
    (rlp/encode [(rlp/encode (nibble/pack (nibble/terminate (:key node))))
                 (rlp/encode (:value node))]
                {:raw? true}))
  Extension
  (->rlp [node]
    (rlp/encode [(rlp/encode (nibble/pack (:key node))) (:child node)]
                {:raw? true})))
