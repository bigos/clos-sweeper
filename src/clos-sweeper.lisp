;;;; clos-sweeper.lisp

(in-package #:clos-sweeper)



(defun window ()
  (let ((app (gtk:make-application :application-id "org.bigos.gtk4-example.better-menu"
                                   :flags gio:+application-flags-flags-none+)))
    (window-activation app)
    (setf *app* app)

    (let ((status (gtk:application-run app nil)))
      (setf *canvas* nil
            *app*    nil)

      (gobj:object-unref app)
      status)))

(defun main ()
  (init-model)
  (window))
