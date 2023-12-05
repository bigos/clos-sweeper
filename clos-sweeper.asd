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
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "globals")
               (:file "type-checking")
               (:file "classes")
               (:file "events")
               (:file "gui-drawing")
               (:file "menu")
               (:file "gtk4")
               (:file "clos-sweeper")))

;; (push #p"/home/jacek/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)
;; (ql:quickload :clos-sweeper)
;; (clos-sweeper::main)
