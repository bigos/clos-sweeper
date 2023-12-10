;;; run it in the terminal with:
;;; sbcl --load  ~/Programming/Lisp/clos-sweeper/launch.lisp


(defun launch ()
  (format t "running launcher on ~A~%" (uiop/os:detect-os))

  (if (eq (uiop/os:detect-os) :os-windows)
      (progn
        (format T "registering code with Windows quicklisp~%")
        (push #p"c:/Users/jacek/Programming/Lisp/clos-sweeper/" asdf:*central-registry*))
      (progn
        (format T "registering code with Linux quicklisp~%")
        (push #p"~/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)))

  (format t "loading clos sweeper~%")
  (ql:quickload :clos-sweeper)

  (format t "RUNNING clos sweeper~%")
  (uiop:symbol-call :clos-sweeper :main)

  (sb-ext:exit))

(launch)
