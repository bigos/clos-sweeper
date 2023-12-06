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
  ;; we need to init model for the global *model* variable
  (clos-sweeper::init-model)
  (let ((b (clos-sweeper::build-grid)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 8 8) (length (clos-sweeper::children b)))))
  (let ((b (clos-sweeper::build-grid 8)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 8 8) (length (clos-sweeper::children b)))))

  (let ((b (clos-sweeper::build-grid 16)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 16 16) (length (clos-sweeper::children b)))))

  (let ((b (clos-sweeper::build-grid 32)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 32 32) (length (clos-sweeper::children b)))))
  ;; find error detection
  (signals (clos-sweeper::build-grid 15)))
