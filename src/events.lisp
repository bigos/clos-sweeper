(in-package #:clos-sweeper)

;;; ================================= All GUI Events ===========================

;;; these functions are for REPL convenience to play with handling the events
(defun de-menu-simple (action-name)           (process-event :menu-simple action-name))
(defun de-menu-bool   (action-name bool01)    (process-event :menu-bool   action-name bool01))
(defun de-menu-radio  (action-name radiostr)  (process-event :menu-radio  action-name radiostr))
(defun de-motion      (x y)                   (process-event :motion  x y))
(defun de-enter       (x y)                   (process-event :enter   x y))
(defun de-leave       ()                      (process-event :leave))
(defun de-pressed     (button x y)            (process-event :pressed   button x y))
(defun de-released    (button x y)            (process-event :released  button x y))
(defun de-scroll      (dx dy)                 (process-event :scroll    dx dy))
(defun de-key-pressed (letter name code mods) (process-event :key-pressed  letter name code mods))
(defun de-resize      (width height)          (process-event :resize width height))
(defun de-timeout     ()                      (process-event :timeout))

;;; ======================= update and view ====================================

(let ((motion-count 0))
  (defun process-event (event &rest args)
    (if (eq event :motion)
        (incf motion-count)
        (setf motion-count 0))

    (unless (member event  '(:motion :timeout))
      (format t "~&processing event ~S ~S ~S~%" event (first args) (rest args)))
    ;; like in Elm runtime calling the update
    (ecase event
      (:menu-simple (progn
                      (setf (selection *model*) (format nil "~S" args))
                      (cond
                        ((equalp (first args) "about")
                         (present-about-dialog))
                        ((equalp (first args) "quit")
                         (close-all-windows-and-quit)))))
      (:menu-bool (progn
                    (setf (selection *model*) (format nil "~S" args))))
      (:menu-radio  (progn
                      (setf (selection *model*) (format nil "~S" args))
                      (when (equalp (first args) "new-game-size")
                        (init-model (width *model*)
                                    (height *model*)
                                    (cond ((equalp (cadr args) "SMALL")  8)
                                          ((equalp (cadr args) "MEDIUM") 16)
                                          ((equalp (cadr args) "LARGE")  32)
                                          (T
                                           (warn "Unexpected size ~A, defaulting to SMALL" (cadr args))
                                           8)))
                        (process-event :resize (width *model*) (height *model*)))))
      (:motion (set-mouse *model* args))
      (:enter  (set-mouse *model* args))
      (:leave  (set-mouse *model* args))
      (:pressed (ecase (first args)
                  (1 (progn
                       (setf (button-1 *model*) T)
                       (process-pressed-1)))
                  (2 (setf (button-2 *model*) T))
                  (3 (progn (setf (button-3 *model*) T)
                            (process-pressed-3)))))
      (:released (ecase (first args)
                   (1 (setf (button-1 *model*) nil))
                   (2 (setf (button-2 *model*) nil))
                   (3 (setf (button-3 *model*) nil))))
      (:scroll )
      (:key-pressed )
      (:resize (progn
                 (setf (width  *model*) (first  args)
                       (height *model*) (second args))
                 (resize-grid (grid *model*))))
      (:timeout)
      )

    ;; inspired by Gtk4, think of separate from update layout phase
    ;; https://docs.gtk.org/gtk4/drawing-model.html
    ;; geometry changes calculated


    ;; like in Elm runtime calling the view
    (if *canvas*
        (gtk4:widget-queue-draw *canvas*)
        (simulate-draw-func (if (width *model*)  (width *model*)  800)
                            (if (height *model*) (height *model*) 600)))))

(defun mine-stats ()
  (let ((covered-mines (length (loop for c in (children (grid *model*))
                                     when (member (state c) '(:covered-mine))
                                       collect c))))
    (setf (selection *model*)
          (cond
            ((not (zerop (length (loop for c in (children (grid *model*))
                                       when (member (state c) '(:exploded-mine))
                                         collect c)) ))
             (format nil "You lost!!!"))
            ((zerop covered-mines)
             (format nil "You won!!!"))
                (T
                 (format nil "Uncover ~A mines more" covered-mines))))))

(defun process-pressed-1 ()
  (let ((box-under-mouse
          (first
           (remove-if-not
            (lambda (c) (mouse-over-p c))
            (children (grid *model*))))))
    (when box-under-mouse
      (cond ((null (state box-under-mouse))
             (setf (state box-under-mouse) :empty))
            ((eq :covered-mine (state box-under-mouse))
             (setf (state box-under-mouse) :exploded-mine)))))
  (mine-stats))

(defun process-pressed-3 ()
  (let ((box-under-mouse
          (first
           (remove-if-not
            (lambda (c) (mouse-over-p c))
            (children (grid *model*))))))
    (when box-under-mouse
      (cond ((null (state box-under-mouse))
             (setf (state box-under-mouse) :flagged-empty))
            ((eq :covered-mine (state box-under-mouse))
             (setf (state box-under-mouse) :flagged-mine))

            ((eq :flagged-empty (state box-under-mouse))
             (setf (state box-under-mouse) nil))
            ((eq :flagged-mine (state box-under-mouse))
             (setf (state box-under-mouse) :covered-mine)))))
  (mine-stats))
