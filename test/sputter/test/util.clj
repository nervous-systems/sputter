(ns sputter.test.util
  (:require [sputter.util         :as util]
            [sputter.tx           :as tx]
            [sputter.storage      :as storage]
            [sputter.storage.stub :as storage.stub]
            [sputter.word         :as word]
            [sputter.vm           :as vm]
            [cheshire.core        :as json]
            [clojure.java.io      :as io]
            [clojure.test         :as test :refer [is]]
            [clojure.string       :as str]))

(def ^:private test-path "externals/ethereum-tests/VMTests")

(defn- hex-literal? [x]
  (and (string? x) (str/starts-with? x "0x")))

(let [renames {"gasPrice"          "gas-price"
               "currentCoinbase"   "coinbase"
               "currentGasLimit"   "gas-limit"
               "currentNumber"     "number"
               "currentDifficulty" "difficulty"
               "currentTimestamp"  "timestamp"}]
  (defn- ->test-key [k]
    (if (hex-literal? k)
      k
      (keyword (renames k k)))))

(defn- file->tests [f]
  (-> f io/reader (json/parse-stream ->test-key)))

(defn- ->test-suite [& subpath]
  (let [path    (str test-path "/" (str/join "/" subpath))
        entries (->> path io/file file-seq)]
    (into []
      (comp
       (remove #(.isDirectory %))
       (mapcat file->tests))
      entries)))

(def ^:private vm-tests (delay (->test-suite)))

(defn- named-tests [re]
  (into []
    (for [[test-name test] @vm-tests
          :when (re-find re (name test-name))]
      [test-name test])))

(defn- hex->biginteger [s]
  (if (= s "0x")
    (biginteger 0)
    (biginteger (util/hex->bytes s))))

(defn- hex->word [s]
  (if (or (not s) (= s "0x"))
    word/zero
    (word/->Word s)))

(defn- ->storage-map [m]
  (util/for-map [[pos w] m]
    (hex->word pos) (hex->word w)))

(defn- ->storage-maps [addr->m]
  (util/for-map [[addr m] addr->m]
    (hex->word addr) (->storage-map (:storage m))))

(defn- map->Storage [m]
  (storage.stub/->Storage (->storage-maps m)))

(defn- test->tx [t]
  (let [exec (:exec t)]
    (tx/map->Transaction
     {:program (vm/disassemble (:code exec))
      :gas     (hex->biginteger (:gas exec))
      :message {:recipient (word/->Word (:address exec))}
      :storage (map->Storage (:pre t))})))

(defn- assert-gas [test exp tx]
  (when-let [exp (some-> exp hex->biginteger)]
    (is (= exp (:gas tx))
        (str test ": Gas value mismatch. " exp " != " (:gas tx)))))

(let [zero (biginteger 0)]
  (defn- assert-return [test exp tx]
    (let [exp (hex->word exp)
          act (word/->Word (:sputter/return tx []))]
      (is (= exp act)
          (str test ": Return value mismatch. " exp " != " act)))))

(defn- assert-error [test exp tx]
  (if (:sputter/error tx)
    (is (nil? (:gas exp))
        (str test ": Wants gas, but got error " (:sputter/error tx)))
    (is (:gas exp) (str test ": No gas value?"))))

(defn- assert-storage [test exp tx]
  (doseq [[addr pos->w] exp]
    (doseq [[pos w] pos->w :let [act-w (storage/retrieve tx addr pos)]]
      (is (= w act-w)
          (str test ": Storage value mismatch for "
               addr ": " w " != " act-w)))))

(defn- run-vm-test [test-name test]
  (let [tx (test->tx test)
        post  (vm/execute tx)]
    (assert-error   test-name test post)
    (assert-gas     test-name (:gas test) post)
    (assert-storage test-name (map->Storage (:post test)) post)
    (assert-return  test-name (:out test) post)))

(defn run-vm-tests [re]
  (doseq [[test-name test] (named-tests re)]
    (println "Running: " (name test-name))
    (run-vm-test test-name test)
    (is true)))
