;;; sbcl --load build-linux-executable.lisp

(push #p"~/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)

(ql:quickload :clos-sweeper)
(use-package :clos-sweeper)

(sb-ext:save-lisp-and-die "win-clos-sweeper"
                          :toplevel #'main
                          :executable t)
