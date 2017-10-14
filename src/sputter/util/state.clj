(ns sputter.util.state)

(defn guard [state f & args]
  (if (:sputter/error state)
    state
    (apply f state args)))

(defmacro state-> [x & forms]
  (let [forms (for [f forms]
                (if (seq? f)
                  `(guard ~@f)
                  (list guard f)))]
    `(-> ~x ~@forms)))
