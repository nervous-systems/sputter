(ns sputter.test.util
  (:require [sputter.util               :as util]
            [sputter.state              :as state]
            [sputter.state.memory       :as mem]
            [sputter.state.storage      :as storage]
            [sputter.state.storage.stub :as storage.stub]
            [sputter.word               :as word]
            [sputter.vm                 :as vm]
            [cheshire.core              :as json]
            [clojure.java.io            :as io]
            [clojure.test               :as test :refer [is]]
            [clojure.walk               :as walk]
            [clojure.string             :as str]))

(def ^:private test-path "externals/ethereum-tests/VMTests")

(defn hex-literal? [x]
  (and (string? x) (str/starts-with? x "0x")))

(let [renames {"gasPrice"          "gas-price"
               "currentCoinbase"   "coinbase"
               "currentGasLimit"   "gas-limit"
               "currentNumber"     "number"
               "currentDifficulty" "difficulty"
               "currentTimestamp"  "timestamp"}]
  (defn ->test-key [k]
    (if (hex-literal? k)
      k
      (keyword (renames k k)))))

(defn file->tests [f]
  (-> f io/reader (json/parse-stream ->test-key)))

(defn ->test-suite [& subpath]
  (let [path    (str test-path "/" (str/join "/" subpath))
        entries (->> path io/file file-seq)]
    (into {}
      (comp
       (remove #(.isDirectory %))
       (mapcat file->tests))
      entries)))

(def all-tests (delay (->test-suite)))

(defn named-tests [re]
  (into (sorted-map)
    (for [[test-name test] @all-tests
          :when (re-find re (name test-name))]
      [test-name test])))

(defn- hex->biginteger [s]
  (biginteger (util/hex->bytes s)))

(defn- ->storage-map [m]
  (util/for-map [[pos w] m]
    (hex->biginteger pos) (word/->Word w)))

(defn- ->storage-maps [addr->m]
  (util/for-map [[addr m] addr->m]
    (word/->Word addr) (->storage-map (:storage m))))

(defn- map->storage [m]
  (storage.stub/->Storage (->storage-maps m)))

(defn test->state [t]
  (let [exec (:exec t)]
    (state/map->State
     {:program (vm/disassemble (:code exec))
      :gas     (hex->biginteger (:gas exec))
      :message {:recipient (word/->Word (:address exec))}
      :storage (map->storage (:pre t))})))

(defn assert-gas [test exp state]
  (when-let [exp (some-> exp hex->biginteger)]
    (is (= exp (:gas state))
        (str test ": Gas value mismatch. " exp " != " (:gas state)))))

(defn assert-error [test exp state]
  (if (:sputter/error state)
    (is (nil? (:gas exp))
        (str test ": Wants gas, but got error " (:sputter/error state)))
    (is (:gas exp) (str test ": No gas value?"))))

(defn assert-mem [state exp]
  (let [exp-storage (:storage exp)
        mem         (:memory state)]
    (doseq [[addr word] exp-storage
            :let [addr (word/->Word addr)
                  exp  (word/->Word word)
                  act  (mem/recall mem addr)]]
      (when (not= exp act)
        (throw (ex-info (str "Memory value mismatch: " exp " != " act)
                        {::vals [exp act]}))))))

(defn assert-storage [test exp state]
  (doseq [[addr pos->w] exp :let [act-count (storage/stored state addr)]]
    (is (= (count pos->w) act-count)
        (str test ": Storage count mismatch for "
             addr ": " (count pos->w) " != " act-count))
    (doseq [[pos w] pos->w :let [act-w (storage/retrieve state addr pos)]]
      (is (= w act-w)
          (str test ": Storage value mismatch for "
               addr ": " w " != " act-w)))))

(defn run-vm-test [test-name test]
  (let [state (test->state test)
        post  (vm/execute state)]
    (assert-error   test-name test post)
    (assert-gas     test-name (:gas  test) post)
    (assert-storage test-name (map->storage (:post test)) post)))

(defonce last-error (atom nil))

(defn run-vm-tests [re]
  (try
    (doseq [[test-name test] (named-tests re)]
      (println "Running: " (name test-name))
      (run-vm-test test-name test)
      (is true))
    (catch Exception e
      (reset! last-error (or (ex-data e) e))
      (throw e))))