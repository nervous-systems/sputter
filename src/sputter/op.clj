(ns sputter.op
  (:require [pandect.algo.sha3-256 :as sha]
            [sputter.op.table      :as op.table]
            [sputter.word          :as word]
            [sputter.tx.memory     :as mem]
            [sputter.util.memory   :as util.mem]
            [sputter.tx            :as tx]
            [sputter.storage       :as storage]))

(defmulti operate (fn [tx op] (::mnemonic op)))

(defmethod operate :default [tx op]
  (assoc tx :sputter/error :not-implemented))

(derive ::jumpdest ::no-op)
(derive ::pop      ::no-op)

(defmethod operate ::no-op [tx op]
  tx)

(defn- simple-op [mnemonic f]
  (defmethod operate mnemonic [tx op]
    (tx/push tx (apply f (::popped op)))))

(defn- zero-guard [f]
  (fn [& args]
    (if (word/zero? (last args))
      word/zero
      (apply f args))))

(simple-op ::add word/add)
(simple-op ::sub word/sub)
(simple-op ::mul word/mul)
(simple-op ::div (zero-guard word/div))
(simple-op ::mod (zero-guard word/mod))
(simple-op ::or  word/or)
(simple-op ::gt  (fn [x y] (if (< 0 (.compareTo x y)) word/one word/zero)))
(simple-op ::lt  (fn [x y] (if (< (.compareTo x y) 0) word/one word/zero)))

(simple-op ::addmod (zero-guard word/add))
(simple-op ::mulmod (zero-guard word/mul))

(defmethod operate ::dup [tx op]
  (-> (reduce tx/push tx (::popped op))
      (tx/push (last (::popped op)))))

(defmethod operate ::mload [tx op]
  (let [[pos]        (::popped op)
        [tx word] (util.mem/recall-word tx pos)]
    (tx/push tx word)))

(defmethod operate ::mstore [tx op]
  (let [store (case (::variant op)
                8   util.mem/insert-byte
                nil util.mem/insert-word)]
    (apply store tx (::popped op))))

(defmethod operate ::msize [tx op]
  (let [n-bytes (* (mem/words tx) word/size)]
    (tx/push tx (word/->Word n-bytes))))

(defmethod operate ::return [tx op]
  (let [[tx i] (apply mem/recall tx (::popped op))]
    (assoc tx :sputter/return i)))

(defmethod operate ::swap [tx op]
  (let [[h & t] (::popped op)
        t       (reverse t)]
    (as-> tx s
      (tx/push s h)
      (reduce tx/push s (rest t))
      (tx/push s (first t)))))

(defmethod operate ::sstore [tx op]
  (apply storage/store tx :recipient (::popped op)))

(defmethod operate ::sload [tx op]
  (let [[pos] (::popped op)]
    (storage/retrieve tx :recipient pos)))

(defmethod operate ::push [tx op]
  (tx/push tx (::data op)))

(defn- jump* [tx target]
  (let [tx' (tx/position tx target)]
    (if (not= (::mnemonic (tx/instruction tx')) ::jumpdest)
      (assoc tx :sputter/error :invalid-jump)
      (assoc tx' :sputter/advance? false))))

(defmethod operate ::jump [tx op]
  (->> op ::popped first (jump* tx)))

(defmethod operate ::jumpi [tx op]
  (let [[pos v] (::popped op)]
    (cond-> tx (not (word/zero? v)) (jump* pos))))
