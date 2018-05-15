(ns sputter.state.trie.util
  (:refer-clojure :exclude [flatten])
  (:require [sputter.state.trie.node :as node]
            [sputter.state.trie.rlp  :as trie.rlp]
            [sputter.util.rlp        :as rlp]
            [sputter.util            :as util]
            [sputter.state.kv        :as kv]))

(def ^:private hash-bytes 32)

(defn inline-node? [rlp & [{:keys [root?]}]]
  (and (not root?) (< (count rlp) hash-bytes)))

(defn- hash->vector [hash store]
  (-> (kv/retrieve store hash)
      rlp/decode))

(defn bytes->node [rlp]
  (-> (rlp/decode rlp)
      trie.rlp/vector->node))

(defn realize [child store]
  (if (node/node? child)
    child
    (some-> (rlp/decode child)
            (cond-> (not (rlp/vector? child)) (hash->vector store))
            trie.rlp/vector->node)))

(defn- walk-path* [node k outer inner]
  (if-let [[child tail] (node/descend node k)]
    (outer (node/reattach node k (inner child tail)) k)
    (outer node k #{:terminal?})))

(defn walk-path [f store node k]
  (walk-path* (realize node store) k f (partial walk-path f store)))

(defn walk-realized [f node]
  (cond-> node
    (node/node? node) (node/walk f (partial walk-realized f))))

(defn- flatten-one [writes node]
  (let [rlp (trie.rlp/->rlp node)]
    (if (inline-node? rlp)
      rlp
      (let [hash-k (util/sha3-bytes rlp)]
        (swap! writes assoc hash-k rlp)
        (rlp/encode hash-k)))))

(defn flatten [root]
  (let [writes (atom {})]
    (node/walk
     root
     (fn finish [root]
       (let [rlp    (trie.rlp/->rlp root)
             hash-k (util/sha3-bytes rlp)]
         [(swap! writes assoc hash-k rlp) hash-k root]))
     (partial walk-realized (partial flatten-one writes)))))
