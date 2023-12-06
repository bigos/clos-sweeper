;; https://lispcookbook.github.io/cl-cookbook/testing.html

(in-package #:clos-sweeper-test)

(def-suite clos-sweeper-suite
  :description "suite to hold other suites and tests")

(in-suite clos-sweeper-suite)

(test test-equality
      "test some very basic equalities"
      (is (= 2 2))
      (is (= 4 (* 2 2))))

(test grid-building
      "test building grid without the arguments"
  (let ((b (build-grid)))
    (is (not (null b)))
    (is (type-of b) 'grid)))
