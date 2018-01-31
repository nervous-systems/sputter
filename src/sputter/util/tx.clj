(ns sputter.util.tx)

(defn guard [tx f & args]
  (if (:sputter/error tx)
    tx
    (apply f tx args)))

(defmacro tx-> [x & forms]
  (let [forms (for [f forms]
                (if (seq? f)
                  `(guard ~@f)
                  (list guard f)))]
    `(-> ~x ~@forms)))
