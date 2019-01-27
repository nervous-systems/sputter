(ns sputter.state.trie
  (:require [sputter.state.kv         :as kv]
            [sputter.state.kv.leveldb :as leveldb]
            [sputter.state.trie.node  :as node]
            [sputter.state.trie.util  :as util :refer [realize]]
            [sputter.util.nibble      :as nibble]
            [sputter.util.rlp         :as rlp]
            [sputter.state.trie.rlp   :as trie.rlp]
            [sputter.util             :refer
             [bytes->hex hex->bytes sha3-bytes]])
  (:import [java.util Arrays]))

(defprotocol Trie
  (-search [trie k])
  (-insert [trie k v])
  (commit  [trie]))

(defn- attach* [v node k & [opts]]
  (cond-> node (:terminal? opts) (node/attach k v)))

(defn- search-below [store node k]
  (lazy-seq
   (cons [node k]
         (when-let [[child rest-k] (node/descend node k)]
           (search-below store (realize child store) rest-k)))))

(defrecord KVTrie [root store]
  Trie
  (-insert [this k v]
    (let [root (util/walk-path (partial attach* v) store root k)]
      (assoc this :root root)))
  (commit [this]
    (let [[writes hash root] (util/flatten root)]
      (if (empty? writes)
        this
        (assoc this
          :root  root
          :store (kv/insert-batch store writes)
          :hash  hash))))
  (-search [this k]
    (search-below store root k)))

(defn map->KVTrie [opts]
  (let [store (:store opts)
        node  (some-> (:root opts)
                      rlp/encode
                      (util/realize store))]
    (KVTrie. node store)))

(defn insert [t k v]
  (-insert t (nibble/->nibbles k) v))

(defn search [t k]
  (let [path (-search t (nibble/->nibbles k))]
    (when-let [v (apply node/value (last path))]
      (cond-> v (string? v) (.getBytes "UTF-8")))))

(defn- proof-flatten-child [node]
  (let [rlp (trie.rlp/->rlp node)]
    (cond-> rlp (not (util/inline-node? rlp)) sha3-bytes)))

(defn- proof-flatten [node]
  (node/walk node trie.rlp/->rlp
             (partial util/walk-realized proof-flatten-child)))

(defn prove [trie k]
  (let [rlp-nodes (map (comp trie.rlp/->rlp first) (-search trie k))]
    (into [(first rlp-nodes)]
      (filter (complement util/inline-node?) (rest rlp-nodes)))))

(defn- descend-inline [node k]
  (if-let [[child rest-k] (node/descend node k)]
    (if (util/inline-node? child)
      (recur (util/bytes->node child) rest-k)
      [(rlp/decode child) rest-k])
    [nil (node/value node k)]))

(defn verify [hash k [head & rest-proof]]
  (if-not (and head (Arrays/equals hash (sha3-bytes head)))
    ::invalid
    (let [node          (util/bytes->node head)
          [next-hash v] (descend-inline node k)]
      (if next-hash
        (recur next-hash v rest-proof)
        (if rest-proof
          ::invalid
          v)))))
