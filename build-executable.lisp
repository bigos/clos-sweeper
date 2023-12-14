;;; sbcl --load build-executable.lisp

(if (eq (uiop/os:detect-os) :os-windows)
    (push #p"c:/Users/jacek/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)
    (push #p"~/Programming/Lisp/clos-sweeper/" asdf:*central-registry*))

(asdf:make :clos-sweeper)
