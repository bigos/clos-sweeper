;;; sbcl --load build-windows-executable.lisp

(push #p"c:/Users/jacek/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)

(ql:quickload :clos-sweeper)
(use-package :clos-sweeper)

(sb-ext:save-lisp-and-die "win-clos-sweeper.exe"
                          :toplevel #'main
                          :executable t)
