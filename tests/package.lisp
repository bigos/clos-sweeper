(defpackage #:clos-sweeper-test
  (:use #:cl
        #:fiveam
        #:clos-sweeper)
  (:export #:run!
           #:run-all-tests
           #:clos-sweeper-suite)
  (:import-from #:clos-sweeper
   :children
   :build-grid
   :init-model))
