;;;; clos-sweeper.asd

(uiop:register-image-restore-hook
 (lambda ()
   (let* ((namespace "Gtk")
          (package (find-package (string-upcase namespace))))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace namespace "4.0"))))))

(asdf:defsystem #:clos-sweeper
    :description "Clos-sweeper is a simple mine sweeper game"
    :author "ruby.object@googlemail.com"
    :license  "Public Domain"
    :version "0.0.1"
    :depends-on (#:alexandria
                 #:serapeum
                 #:defclass-std
                 #:cl-gtk4 #:cl-gdk4 #:cl-glib #:cl-cairo2)
    ;; --------------------------------
    :defsystem-depends-on (:deploy)
    :build-operation "deploy-op"
    :build-pathname "clos-sweeper"
    :entry-point "clos-sweeper:main"
    ;; --------------------------------
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

;; (push #p"~/Programming/Lisp/clos-sweeper/" asdf:*central-registry*)
;; (ql:quickload :clos-sweeper)
;; (in-package :clos-sweeper)
;; (main)
;; (ql:quickload :clos-sweeper/tests)
;; (clos-sweeper-test:run! 'clos-sweeper-test:clos-sweeper-suite)

(asdf:defsystem #:clos-sweeper/tests
    :depends-on (#:clos-sweeper #:fiveam)
    :pathname "tests/"
    :components ((:file "package")
                 (:file "clos-sweeper-tests"))
    :perform (test-op (op c)
                      (uiop:symbol-call :fiveam :run!
                                        (uiop:find-symbol*
                                         :clos-sweeper-suite
                                         :clos-sweeper-test))))
;; (ql:quickload :clos-sweeper/tests)
;; (in-package #:clos-sweeper-test)
;; (run! 'clos-sweeper-suite)
