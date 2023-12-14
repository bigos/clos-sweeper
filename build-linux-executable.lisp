;;; sbcl --load build-linux-executable.lisp

(push #p"~/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)

(asdf:make :clos-sweeper)
