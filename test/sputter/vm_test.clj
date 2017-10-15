(ns sputter.vm-test
  (:require [clojure.test :refer [deftest]]
            [sputter.test.util :as test.util] :reload))

(deftest add
  (test.util/run-vm-tests #"^add\d+$"))

(deftest addmod
  (test.util/run-vm-tests #"^addmod[^_]+$"))

(deftest sub
  (test.util/run-vm-tests #"^sub\d+$"))

(deftest mul
  (test.util/run-vm-tests #"^mul(?!mod)"))

(deftest mulmod
  (test.util/run-vm-tests #"^mulmod[^_]+$"))

(deftest div
  (test.util/run-vm-tests #"^div"))

(deftest mod*
  (test.util/run-vm-tests #"^mod"))

(deftest or*
  (test.util/run-vm-tests #"^or\d+"))

(deftest gt
  (test.util/run-vm-tests #"^gt\d+"))

(deftest lt
  (test.util/run-vm-tests #"^lt\d+"))

(deftest dup
  (test.util/run-vm-tests #"^dup"))

(deftest swap
  (test.util/run-vm-tests #"^swap"))

(deftest push
  (test.util/run-vm-tests #"^push(?!32AndSuicide)"))

(deftest mload
  (test.util/run-vm-tests #"^mload"))

(deftest mstore
  (test.util/run-vm-tests #"^mstore"))

(deftest return
  (test.util/run-vm-tests #"^return"))
