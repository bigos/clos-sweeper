;;; run it in the terminal with:
;;; sbcl --load  ~/Programming/Lisp/clos-sweeper/testing.lisp


(defun testing ()
  (format t "running testing~%")

  (if (eq (uiop/os:detect-os) :os-windows)
      (push #p"c:/Users/jacek/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)
      (push #p"~/Programming/Lisp/clos-sweeper/" asdf:*central-registry*))
  (ql:quickload :clos-sweeper)
  (ql:quickload :clos-sweeper/tests)
  (asdf:test-system :clos-sweeper/tests)

  (sb-ext:exit))

(testing)
