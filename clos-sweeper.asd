;;;; clos-sweeper.asd

(asdf:defsystem #:clos-sweeper
  :description "Describe clos-sweeper here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:serapeum
               #:defclass-std
               #:cl-gtk4 #:cl-gdk4 #:cl-glib #:cl-cairo2)
  :serial t
  :components ((:file "package")
               (:file "clos-sweeper")))

;; (push #p"/home/jacekp/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)
;; (ql:quickload :clos-sweeper)
;; (clos-sweeper::main)
