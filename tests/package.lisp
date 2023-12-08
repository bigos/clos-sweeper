(defpackage #:clos-sweeper-test
  (:use #:cl
        #:fiveam
        #:clos-sweeper)
  (:export #:run!
           #:run-all-tests
           #:clos-sweeper-suite)
  (:import-from #:clos-sweeper
   :*model*
   :build-grid
   :grid
   :state
   :children
   :de-resize
   :de-pressed
   :de-released
   :de-enter
   :de-motion
   :middle
   :height
   :init-model
   :selection
   :simulate-draw-func
   :width
   ))
