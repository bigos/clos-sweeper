(in-package #:clos-sweeper-test)

(def-suite clos-sweeper-suite
  :description "suite to hold other suites and tests")

(in-suite clos-sweeper-suite)

(test test-equality
      "test some equalities"
      (is (= 2 2))
      (is (= 4 (* 2 2)))
      (is (equal (tryme) "Thank you for trying.")))
