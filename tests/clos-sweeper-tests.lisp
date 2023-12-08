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
  (init-model)
  (let ((b (build-grid)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 8 8) (length (children b)))))
  (let ((b (build-grid 8)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 8 8) (length (children b)))))

  (signals error (build-grid 15))

  (let ((b (build-grid 16)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 16 16) (length (children b)))))

  (signals error (build-grid 17))

  (let ((b (build-grid 32)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 32 32) (length (children b)))))

  (signals error (build-grid 15)))


(test grid-clicking
  "test building grid without the arguments"
  ;; we need to init model for the global *model* variable
  (init-model)
  (de-resize 400 500)

  (let ((b (build-grid)))
    (is (not (null b)))
    (is (type-of b) 'grid)
    (is (= (* 8 8) (length (children b)))))

  (de-enter 0 0)
  (is (equalp (selection *model*) "start uncovering mines"))
  (let ((lmb 1)
        (rmb 3)
        (empty-ones   (serapeum:filter (lambda (c) (null (state c))) (children (grid *model*))))
        (hidden-mines (serapeum:filter (lambda (c) (eq (state c) :covered-mine)) (children (grid *model*)))))
    (is (= 64 (+ (length empty-ones) (length hidden-mines))))

    (apply #'de-motion (middle (first empty-ones)))
    (is (equalp (selection *model*) "start uncovering mines"))
    (is (eq (state (first empty-ones)) nil))
    (apply #'de-pressed  (cons lmb  (middle (first empty-ones))))
    (apply #'de-released (cons lmb  (middle (first empty-ones))))
    (is (eq (state (first empty-ones)) :empty))
    (is (equalp (selection *model*) "Uncover 16 mines more"))

    (apply #'de-motion (middle (first hidden-mines)))
    (is (equalp (selection *model*) "Uncover 16 mines more"))
    (is (eq (state (first hidden-mines)) :covered-mine))
    (apply #'de-pressed  (cons rmb  (middle (first hidden-mines))))
    (apply #'de-released (cons rmb  (middle (first hidden-mines))))
    (is (eq (state (first hidden-mines)) :flagged-mine))
    (is (equalp (selection *model*) "Uncover 15 mines more"))

    (loop for c in (rest empty-ones) do
      (apply #'de-motion (middle c))
      (is (eq (state c) nil))
      (apply #'de-pressed  (cons lmb  (middle c)))
      (apply #'de-released (cons lmb  (middle c)))
      (is (eq (state c) :empty)))

    (is (equalp (selection *model*) "Uncover 15 mines more"))

    (loop for c in (rest hidden-mines) do
      (apply #'de-motion (middle c))
      (is (eq (state c) :covered-mine))
      (apply #'de-pressed  (cons rmb  (middle c)))
      (apply #'de-released (cons rmb  (middle c)))
      (is (eq (state c) :flagged-mine)))

    (is (equalp (selection *model*) "You won!!!"))
    )
  (simulate-draw-func (width *model*) (height *model*)))
