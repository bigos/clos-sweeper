(in-package #:clos-sweeper)


;; =========================== dialogs =========================================
(defun present-about-dialog ()
  (let ((dialog (about-dialog)))
    (setf (gtk4:window-modal-p dialog) t
          (gtk4:window-transient-for dialog) (gtk4:application-active-window *app*))
    (gtk4:window-present dialog)))

(defun about-dialog ()
  (let ((dialog (gtk4:make-about-dialog))
        (system (asdf:find-system :clos-sweeper)))
    (setf (gtk4:about-dialog-authors      dialog)   (list "Jacek Podkanski")
          (gtk4:about-dialog-website      dialog)   "https://github.com/bigos/clos-sweeper"
          (gtk4:about-dialog-program-name dialog)   "CLOS Sweeper"
          (gtk4:about-dialog-comments     dialog)   "A sample GUI app written in Lisp"
          (gtk4:about-dialog-license      dialog)   "Public Domain"
          (gtk4:about-dialog-system-information dialog) (format nil "~A ~A"
                                                                (lisp-implementation-type)
                                                                (lisp-implementation-version))
          (gtk4:about-dialog-logo-icon-name dialog) "application-x-addon")
    (values dialog)))

;; =========================== closing everything ==============================
(defun close-all-windows-and-quit ()
  (loop for aw = (gtk4:application-active-window *app*)
        until (null aw)
        do (gtk4:window-close aw)))

;; ============================= key event translation =========================
(defun translate-key-args (args)
  (destructuring-bind (keyval keycode keymods) args
    (list
     (format nil "~A"
             (let ((unicode (gdk:keyval-to-unicode keyval)))
               (if (or (zerop unicode)
                       (member keyval
                               (list gdk:+key-escape+
                                     gdk:+key-backspace+
                                     gdk:+key-delete+)))
                   ""
                   (code-char unicode))))
     (gdk:keyval-name keyval)
     keycode
     (remove-if (lambda (m) (member m '(:num-lock)))
                (loop
                  for modname in '(:shift :caps-lock :ctrl :alt
                                   :num-lock :k6 :win :alt-gr)
                  for x = 0 then (1+ x)
                  for modcode = (mask-field (byte 1 x) keymods)
                  unless (zerop modcode)
                    collect modname)))))

;; ============================ events =========================================
(defun window-events (window)
  (let ((key-controller (gtk4:make-event-controller-key)))
    (gtk4:widget-add-controller window key-controller)
    (gtk4:connect key-controller "key-pressed"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    (apply #'de-key-pressed (funcall #'translate-key-args args)))))

  (glib:timeout-add 1000
                    (lambda (&rest args)
                      (declare (ignore args))
                      (funcall #'de-timeout)
                      glib:+source-continue+))

  (gtk4:connect window "destroy" (lambda (widget &rest args)
                                   (declare (ignore widget args))
                                   (warn "implement function to handle window destruction")
                                   glib:+source-continue+)))

(defun canvas-events (canvas)
  (let ((motion-controller (gtk4:make-event-controller-motion)))
    (gtk4:widget-add-controller canvas motion-controller)

    (gtk4:connect motion-controller "motion"
                  (lambda (e &rest args) (declare (ignore e)) (apply #'de-motion args)))
    (gtk4:connect motion-controller "enter"
                  (lambda (e &rest args) (declare (ignore e)) (apply #'de-enter args)))
    (gtk4:connect motion-controller "leave"
                  (lambda (e &rest args) (declare (ignore e)) (apply #'de-leave args))))

  (let ((scroll-controller (gtk4:make-event-controller-scroll :flags gtk4:+event-controller-scroll-flags-vertical+)))
    (gtk4:widget-add-controller canvas scroll-controller)
    (gtk4:connect scroll-controller "scroll"
                  (lambda (e &rest args) (declare (ignore e)) (apply #'de-scroll args))))

  (let ((gesture-click-controller (gtk4:make-gesture-click))
        (click-fn (lambda (event args click-de-fn)
                    (let ((current-button (gtk4:gesture-single-current-button event)))
                      (apply click-de-fn (list current-button
                                                (nth 1 args)
                                                (nth 2 args)))))))
    ;; make gesture click listen to other mouse buttons as well
    (setf (gtk4:gesture-single-button gesture-click-controller) 0)
    (gtk4:widget-add-controller canvas gesture-click-controller)

    (gtk4:connect gesture-click-controller "pressed"
                  (lambda (event &rest args) (apply click-fn (list event args #'de-pressed))))
    (gtk4:connect gesture-click-controller "released"
                  (lambda (event &rest args) (apply click-fn (list event args #'de-released)))))


  ;; for some reason resize signal does not work without  notify
  (gtk4:connect canvas "notify" (lambda (widget &rest args)
                                  (format t "~&>>>>>>>>>>>>>>>>>>>>>>>> notifying ~S ~S~%" widget args)))

  (gtk4:connect canvas "resize" (lambda (widget &rest args)
                                  (declare (ignore widget))
                                  (de-resize (nth 0 args) (nth 1 args)))))

;; =============================================================================
(defun window-activation (app)
  (gtk4:connect app "activate"
                (lambda (app)
                  (let ((window (gtk4:make-application-window :application app)))
                    (setf
                     (gtk4:application-menubar app) (menu-bar-menu app)
                     (gtk4:application-window-show-menubar-p window) T)

                    (setf
                     (gtk4:window-title window) "CLOS Sweeper"
                     (gtk4:window-default-size window) (list 400 400))

                    (let ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                              :spacing 0)))
                      (let ((canvas (gtk4:make-drawing-area)))
                        (setf *canvas* canvas
                              (gtk4:widget-vexpand-p canvas) T
                              (gtk4:drawing-area-draw-func canvas) (list (cffi:callback %draw-func)
                                                                         (cffi:null-pointer)
                                                                         (cffi:null-pointer)))
                        (canvas-events canvas)
                        (gtk4:box-append box canvas))
                      (setf (gtk4:window-child window) box))

                    (window-events window)

                    (format t "actions defined for app ~A~%"  (gio:action-group-list-actions app))

                    (gtk:window-present window)))))
