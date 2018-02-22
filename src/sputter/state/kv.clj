(ns sputter.state.kv)

(defprotocol KeyValueStore
  (retrieve [store k]
    "Return the value associated with the given key.")
  (insert [store k v]
    "Return a new store, with an additional `k` -> `v` mapping.")
  (delete [store k]
    "Return a new store, without the given key.")
  (insert-batch [store k]
    "Return a new store, with additional mappings."))

(extend-type clojure.lang.IPersistentMap
  KeyValueStore
  (retrieve [m k]
    (get m k))
  (insert [m k v]
    (assoc m k v))
  (delete [m k]
    (dissoc m k))
  (insert-batch [m other]
    (merge m other)))
