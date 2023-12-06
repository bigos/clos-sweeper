;; https://lispcookbook.github.io/cl-cookbook/testing.html

(in-package #:clos-sweeper-test)

(def-suite clos-sweeper-suite
  :description "suite to hold other suites and tests")

(in-suite clos-sweeper-suite)

(test test-equality
  "test some very basic equalities"
  (is (= 2 2))
  (is (= 4 (* 2 2)))
  ;; a little example for checking error hierarchy
  (signals error (/ 1 0))
  (signals arithmetic-error (/ 2 0))
  (signals division-by-zero (/ 3 0)))

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

  (signals error (clos-sweeper::build-grid 15))
  )
