;;; sbcl --load build-windows-executable.lisp

(push #p"c:/Users/jacek/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)

(asdf:make :clos-sweeper)

