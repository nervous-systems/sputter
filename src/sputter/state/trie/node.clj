(ns sputter.state.trie.node
  (:require [sputter.util :as util :refer [split-prefix]]))

(defprotocol Node
  (terminates? [node k]
    "Does this node represent a successful termination of the search
     for the given key remainder?")
  (-value [node]
    "The value of this node, if any.")
  (attach [node k v]
    "Attach the `v` to this node, at the given key.")
  (reattach [node k child]
    "Replace the child node at `k` with `child`.")
  (descend [node candidate-k]
    "Locate the immediate child pointed to by the given key.
     Return vector of `[child remaining-k]` or `nil`.")
  (walk [node outer inner]
    "Apply `inner` to all children, then `outer` to this. Return the result."))

(defn node? [x]
  (extends? Node (class x)))

(declare ->Leaf ->Extension)

(defn value
  "Return the value of the given node, or nil.
   If a key is supplied, return the value when the node matches."
  ([node] (-value node))
  ([node k]
   (when (terminates? node k)
     (-value node))))

(defrecord Branch [branches value]
  Node
  (terminates? [this k] (empty? k))
  (-value      [this]   value)
  (attach [this [nibble & rest-k] v]
    (if nibble
      (assoc-in this [:branches (int nibble)] (->Leaf rest-k v))
      (assoc    this :value v)))
  (reattach [this [nibble] child]
    (assoc-in this [:branches (int nibble)] child))
  (descend [this [nibble & rest-k]]
    (when nibble
      [(branches (int nibble)) rest-k]))
  (walk [this outer inner]
    (outer (update this :branches (partial util/map-values inner)))))

(defn ->Branch
  ([]         (->Branch {}))
  ([branches] (->Branch branches nil))
  ([branches value]
   (Branch. branches value)))

(defrecord Leaf [key value]
  Node
  (terminates? [_ candidate-k] (= candidate-k key))
  (-value      [_]             value)
  (attach [this input-k v]
    (if (= input-k key)
      (assoc this :value v)
      (let [[pre key-suf input-suf] (split-prefix key input-k)]
        (-> (->Branch)
            (attach input-suf v)
            (attach key-suf   value)
            (cond->> pre (->Extension pre))))))
  (descend [_ _] nil)
  (walk [this outer _]
    (outer this)))

(defrecord Extension [key child]
  Node
  (terminates? [_ _] false)
  (-value      [_]   nil)
  (attach   [this input-k v]
    (let [[pre key-suf input-suf] (split-prefix key input-k)
          me                      (if (< 1 (count key-suf))
                                    (assoc this :key (rest key-suf))
                                    child)]
      (-> (->Branch)
          (attach   input-suf v)
          (reattach key-suf   me)
          (cond->> pre (->Extension pre)))))
  (reattach [this _ child]
    (assoc this :child child))
  (descend [this candidate-k]
    (let [[prefix tail] (split-at (count key) candidate-k)]
      (when (= prefix key)
        [child tail])))
  (walk [this outer inner]
    (outer (update this :child inner))))

(extend-type nil
  Node
  (terminates? [_ _]   false)
  (-value      [_]     nil)
  (attach      [_ k v] (->Leaf k v))
  (descend     [_ _]   nil)
  (walk        [_ _ _] nil))
