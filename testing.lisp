;;; run it in the terminal with:
;;; sbcl --load  ~/Programming/Lisp/clos-sweeper/testing.lisp


(defun testing ()
  (format t "running testing~%")

  (push #p"/home/jacek/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)
  (asdf:test-system :clos-sweeper/tests)

  (sb-ext:exit))

(testing)
