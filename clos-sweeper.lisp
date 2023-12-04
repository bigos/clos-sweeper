;;;; clos-sweeper.lisp

(in-package #:clos-sweeper)

;;; ====================== globals =============================================
(defparameter *model*  nil)
(defparameter *canvas* nil)
(defparameter *app* nil)

;; ====================================== type checking in a class ========================
;; https://stackoverflow.com/questions/51723992/how-to-force-slots-type-to-be-checked-during-make-instance/56920918

;;; First the metaclass:
;;; first a metaclass for classes which checks slot writes
(defclass checked-class (standard-class)
  ())

;;; this is a MOP method, probably use CLOSER-MOP for a portable version
(defmethod sb-mop:validate-superclass ((class checked-class) (superclass standard-class))
  t)

;; Now we check all slot writes for that metaclass:
;; this is a MOP method, probably use CLOSER-MOP for a portable version

(defmethod (setf sb-mop:slot-value-using-class) :before (new-value (class checked-class) object slot)
  (if (eq 'T (sb-mop:slot-definition-type slot))
      (when NIL
        (warn "skipping asserting ~S slot ~S"
              class
              (sb-mop:slot-definition-name slot)))
      (progn
        (when NIL
          (warn "asserting ~S slot ~S with type ~S"
                class
                (sb-mop:slot-definition-name slot)
                (sb-mop:slot-definition-type slot)))

        (assert (typep new-value (sb-mop:slot-definition-type slot))
                ()
                "new value ~s is not of type ~a in object ~a slot ~a"
                new-value
                (sb-mop:slot-definition-type slot)
                object
                (sb-mop:slot-definition-name slot)))))

(defun set-all-unbound-slots (instance &optional (value nil))
  (let ((class (class-of instance)))
    (sb-mop:finalize-inheritance class)
    (loop for slot in (sb-mop:compute-slots class)
          for name = (sb-mop:slot-definition-name slot)
          unless (slot-boundp instance name)
            do (setf (slot-value instance name) value))
    instance))

(defclass set-unbound-slots-mixin () ())

(defmethod initialize-instance :after ((i set-unbound-slots-mixin) &rest initargs)
  (declare (ignore initargs))
  (set-all-unbound-slots i nil))


;;; ====================== classes =============================================
(defclass/std model (set-unbound-slots-mixin)
        ((selection :type string)
         (width)
         (height)
         (mouse-x)
         (mouse-y)
         (button-1)
         (button-2)
         (button-3)
         (grid))
        (:metaclass checked-class))

(defun init-model (&optional width height)
  (setf *model* (make-instance 'model
                               :selection "start uncovering mines"
                               :width width
                               :height height))
  (setf (grid *model*) (build-grid))
  *model*)

(defclass/std box (rel)
  ((top-left-x)
   (top-left-y)
   (bottom-right-x)
   (bottom-right-y)
   (state))) ; covered-empty=nil, covered-mine, empty, flagged-mine, exploded-mine

(defclass/std grid (box)
  ((grid-size :std 8)))

(defun build-box (class id x y w h)
  (make-instance class
                 :id id
                 :top-left-x x
                 :top-left-y y
                 :bottom-right-x (+ x w)
                 :bottom-right-y (+ y h)))

;; (add-child  (make-instance 'rel :id 1) (make-instance 'rel :id 2))
(defclass/std rel ()
  ((id)
   (parent)
   (children)))

(defun build-grid ()
  (let* ((offset-x 50)
         (offset-y 20)
         (grid-size 8)
         (dist (* 0.8  (min (or (width *model*) 20) (or (height *model*) 20))))
         (tile-size (* 0.70 dist))
         (mines-count  (floor (/ (expt grid-size 2)
                                 4)))

         (parent (build-box 'grid :GRID offset-x offset-y (* dist grid-size) (* dist grid-size)))) ;that still shows, we need better way for different boxes
    (loop for r from 0 below grid-size do
      (loop for c from 0 below grid-size do
        (let* ((k (list :id r c))
               (box (build-box 'box
                               k
                               (+ offset-x (* r dist))
                               (+ offset-y (* c dist))
                               tile-size tile-size
                               )))
          (add-child parent box))))
    (loop for b in
                (subseq (alexandria:shuffle (children parent)) 0 mines-count)
          do (setf (state b) :covered-mine))
    parent))

;;; ----------------- hash relations -------------------------------------------
(defclass/std hrel ()
  ((id)
   (parent)
   (children :std (make-hash-table))))

(defmethod add-child ((parent hrel) (child hrel))
  (setf (parent child) parent
        (gethash (id child) (children parent)) child))

;; (defun tryme ()
;;   (let ((parent (make-instance 'hrel :id :parent)))
;;     (add-child parent (make-instance 'hrel :id :c1))
;;     (add-child parent (make-instance 'hrel :id :c2))
;;     (add-child parent (make-instance 'hrel :id :c3))
;;     parent))

;;; ======================== methods ===========================================
(defmethod set-mouse ((model model) args)
  (setf (mouse-x model) (when (first args) (values (round (first  args))))
        (mouse-y model) (when (first args) (values (round (second args))))))

(defmethod mouse-over-p ((box box))
  (coordinates-within-p box
                        (mouse-x *model*)
                        (mouse-y *model*)))

(defmethod coordinates-within-p ((box box) x y)
  (and x
       y
       (<= (top-left-x box) x (bottom-right-x box))
       (<= (top-left-y box) y (bottom-right-y box))))

(defmethod width  ((box box))
    (- (bottom-right-x box) (top-left-x box)))

(defmethod height ((box box))
  (- (bottom-right-y box) (top-left-y box)))

(defmethod %rectangle ((box box))
  (cairo:rectangle (top-left-x box)
                   (top-left-y box)
                   (width box)
                   (height box)))

(defmethod neighbours ((box box))
  ;; still includes oneself
  (let* ((current-row    (second (id box)))
         (current-column (third  (id box)))
         (neighbour-rows    (loop for cr in '(-1 0 1) collect (+ current-row    cr)))
         (neighbour-columns (loop for cr in '(-1 0 1) collect (+ current-column cr))))
    (loop for c in (children (parent box))
          for row = (second (id c))
          for col = (third  (id c))
          when (and (member row neighbour-rows)
                    (member col neighbour-columns))
            collect c)))

(defmethod render ((box box))
  ;; https://drafts.csswg.org/css-color/#named-colors
  (set-rgba (if T
                (if (null (state box))
                    "olive"
                    (ecase (state box)
                      (:empty         "green")
                      (:covered-mine  "olive")
                      (:flagged-mine  "pink")
                      (:flagged-empty "purple")
                      (:exploded-mine "orangered")))))
  (%rectangle box)
  (cairo:stroke-preserve)
  (cairo:fill-path)

  ;; TODO add numbers 1-8 for neighbouring mines on empty boxes

  (let ((cnt (format nil "~A" (length
                               (loop for n in  (neighbours box)
                                     when (member (state n)
                                                  '(:covered-mine :flagged-mine))
                                       collect n)))))
    (when (and (member (state box) '(:empty))
               (not (equalp cnt "0")))
      (progn
        (cairo:select-font-face "Ubuntu Mono" :normal :bold)
        (cairo:set-font-size (height box))
        (multiple-value-bind  (xb yb width height)
            (cairo:text-extents cnt)
          (declare (ignore xb yb)))
        (cairo:set-source-rgb 0 0 0)
        (cairo:move-to (1+ (top-left-x box)) (1- (bottom-right-y box)))
        (cairo:show-text (format nil "~A" cnt)))))

  (when (mouse-over-p box)
    (%rectangle box)
    (set-rgba (if (button-1 *model*)
                  "blue"
                  (if (member (state box) '(nil :empty :covered-mine))
                      "orange"
                      "green")))
    (cairo:stroke))

  (loop for c in (children box) do (render c)))

(defmethod render ((box grid))
  (set-rgba (if (mouse-over-p box)
                (if (button-1 *model*)
                    "#eee"
                    "white")
                "#ddd"))

  (%rectangle box)
  (cairo:stroke-preserve)
  (cairo:fill-path)
  (loop for c in (children box) do (render c)))

(defmethod resize-grid ((grid grid))
  (let* ((minsize (min (width *model*) (height *model*)))
         (dist (* 0.90 (/ minsize (grid-size grid))))
         (tile-size (* 0.75 dist))
         (vertical-center (/ (width *model*) 2))
         (offset-x  (- vertical-center  (* (/ (grid-size grid) 2) dist) ))
         (offset-y 25))
    (loop for r from 0 below (grid-size grid) do
      (loop for c from 0 below (grid-size grid) do
        (let* ((box (find (list :id r c)
                          (children grid ) :test #'equalp :key 'id))
               (tx (* 1.0 (+ offset-x (* r dist))))
               (ty (* 1.0 (+ offset-y (* c dist)))))
          (setf (top-left-x box) tx
                (top-left-y box) ty
                (bottom-right-x box) (+ tx tile-size)
                (bottom-right-y box) (+ ty tile-size)))))
    (let ((tl  (find (list :id 0 0) (children grid )
                     :test #'equalp :key 'id))
          (br  (find (list :id (1- (grid-size grid)) (1- (grid-size grid))) (children grid )
                     :test #'equalp :key 'id)))
      (setf (top-left-x grid) (top-left-x tl)
            (top-left-y grid) (top-left-y tl)
            (bottom-right-x grid) (bottom-right-x br)
            (bottom-right-y grid) (bottom-right-y br)))
    grid))

(defmethod add-child ((parent rel) (child rel))
      (setf (parent child) parent
            (children parent) (cons  child (children parent)))
  child)

(defmethod add-child ((parent box) (child box))
  (call-next-method))

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
      (:menu-simple (setf (selection *model*) (format nil "~S" args)))
      (:menu-bool   (setf (selection *model*) (format nil "~S" args)))
      (:menu-radio  (progn
                      (setf (selection *model*) (format nil "~S" args))
                      (when (and (equalp (first args) "new-game-size")
                                 (equalp (rest args) '("SMALL")))
                        (init-model (width *model*) (height *model*))
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
             (format nil "You won!!"))
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

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(cffi:defcstruct gdk-rgba
  (red   :float)
  (green :float)
  (blue  :float)
  (alpha :float))

(defun color-to-rgba (color)
  (cffi:with-foreign-object (rgba '(:struct gdk-rgba))
    (let ((pointer (make-instance 'gir::struct-instance
                                  :class (gir:nget gdk::*ns* "RGBA")
                                  :this rgba)))
      (let ((valid-color (gdk:rgba-parse pointer color)))
        (cffi:with-foreign-slots ((red green blue alpha) rgba (:struct gdk-rgba))
          (list valid-color red green blue alpha))))))

(defun set-rgba (color)
  (let ((parsed-color (color-to-rgba color)))
    (if (first parsed-color)
        (apply 'cairo:set-source-rgba (rest parsed-color))
        (error "~S is not a valid color" color))))
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; drawing callback ===========================================================
(cffi:defcallback %draw-func :void ((area :pointer)
                                    (cr :pointer)
                                    (width :int)
                                    (height :int)
                                    (data :pointer))
  (declare (ignore area data))

  (setf cairo:*context* (make-instance 'cairo:context
                                       :pointer cr
                                       :width width
                                       :height height
                                       :pixel-based-p nil))
  ;; ###########################################################################

  ;; call actual drawing
  (draw-objects (coerce (the (signed-byte 32) width)  'single-float)
                (coerce (the (signed-byte 32) height) 'single-float)))

(defun simulate-draw-func (w h)
  (let* ((surface (cairo:create-image-surface :argb32
                                              w
                                              h)))
    (let* ((ctx (cairo:create-context surface)))
      (setf  cairo:*context* ctx)
      ;; #######################################################################

      ;; call actual drawing
      (draw-objects w
                    h))

    ;; put drawn surface to a file
    (cairo:surface-write-to-png surface
                                (format nil
                                        "/tmp/cairo-simulate-drawing-~A.png"
                                        (get-internal-run-time)))))

;;; ============================ view ==========================================
(defun draw-objects (w h)               ; view
  ;; (format t ">>>>>>>> in draw-objects ~S " cairo:*context*)
  (let ((cnt (format nil "~S" (selection *model*)))
        (tw 0)
        (th 0)
        (tpx 0)
        (tpy 0))
    (cairo:set-source-rgb 1 1 1)
    (cairo:paint)

    (render (grid *model*))


    (cairo:select-font-face "Ubuntu Mono" :normal :bold)
    (cairo:set-font-size 20)
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents cnt)
      (declare (ignore xb yb))
      (setf
       tw width
       th height))
    (setf tpx 6
          tpy 15)
    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to tpx tpy)
    (cairo:show-text (format nil "~A" cnt))




    (let ((bottom-text (format nil "mouse: ~Sx~S, canvas: ~Ax~A"
                               (mouse-x *model*) (mouse-y *model*) w h) ))
      (multiple-value-bind (xp yp width height)
          (cairo:text-extents bottom-text)
        (declare (ignore xp yp))
        (cairo:move-to (- w (* 1.01 width))
                       (- h (* 0.3 height)))
        (cairo:show-text bottom-text)))


    ;; circle at mouse coordinates
    (when (and (null *canvas*)
               (mouse-x *model*)
               (mouse-y *model*))
      (cairo:set-source-rgba 1 1 0.2 0.4)
      (cairo:set-line-width 6.0)
      (cairo:arc (mouse-x *model*)
                 (mouse-y *model*)
                 10.0 0 (* 2.0 pi))
      (cairo:fill-path)
      (cairo:arc (mouse-x *model*)
                 (mouse-y *model*)
                 10.0 0 (* 2.0 pi))
      (set-rgba "green")
      (cairo:set-line-width 0.5)
      (cairo:stroke))


    ))

;; ================================= MENU ======================================
;; https://docs.gtk.org/gio/ctor.MenuItem.new_section.html ; ===================

(defun prepare-radio-action (app action-name default)
  (let ((action
         (gio:make-stateful-simple-action :name action-name
                                          :parameter-type (glib:make-variant-type
                                                           :type-string "s")
                                          :state (glib:make-string-variant
                                                  :string default))))
    (gio:action-map-add-action app action)
    (gtk4:connect action "activate"
                  (lambda (event parameter)
                    (declare (ignore event))
                    (gio:action-change-state action parameter)

                    (apply 'de-menu-radio (list action-name
                                                (glib:variant-string
                                                 (gio:action-state action))))))
    (gobj:object-unref action)))

(defun prepare-item-radio (app menu label action-name string)
  (declare (ignore app))

  (format t "preparing radio item ~S~%" (list label action-name string))

  (let ((item (gio:make-menu-item :model menu
                                  :label label
                                  :detailed-action (format nil "app.~A" action-name))))
    (setf (gio:menu-item-action-and-target-value item)
          (list (format nil  "app.~A" action-name) (glib:make-string-variant
                                       :string string)))
    item))

(defun prepare-item-bool (app menu label action-name default &key (disabled nil))
  (let ((action (gio:make-stateful-simple-action :name action-name
                                                 :parameter-type nil
                                                 :state (glib:make-boolean-variant
                                                         :value default))))
    (when disabled (setf (gio:simple-action-enabled-p action) nil))
    (gio:action-map-add-action app action)

    (gtk:connect action "activate"
                 (lambda (event parameter)
                   (declare (ignore event parameter))
                   (gio:action-change-state action (glib:make-boolean-variant
                                                    :value (if (zerop (glib:variant-hash (gio:action-state action)))
                                                               T
                                                               nil)))
                   (apply 'de-menu-bool
                          (list action-name
                                (glib:variant-hash (gio:action-state action))))))
    (gobj:object-unref action))

  (gio:make-menu-item :model menu
                      :label label
                      :detailed-action (format nil  "app.~A" action-name)))

(defun prepare-item-simple (app menu label action-name &key (disabled nil))
  (let ((action (gio:make-simple-action :name action-name
                                        :parameter-type nil)))
    (when disabled (setf (gio:simple-action-enabled-p action) nil))
    (gio:action-map-add-action app action)

    (gtk4:connect action "activate"
                  (lambda (event parameter)
                    (declare (ignore event parameter))

                    (apply 'de-menu-simple (list action-name))))
    (gobj:object-unref action))

  (gio:make-menu-item :model menu
                      :label label
                      :detailed-action (format nil  "app.~A" action-name)))

(defun prepare-section (label section)
  (gio:make-section-menu-item
   :label label
   :section  section))

(defun prepare-submenu (label &rest submenu-items)
  (list :submenu label
        (apply 'build-items submenu-items)))

(defun build-items (&rest items)
  (let ((submenu (gio:make-menu)))
    (apply 'build-menu submenu items)
    submenu))

(defun build-menu (submenu &rest items)
  (loop for i in items
        for item-class-string = (when (typep i 'gir::object-instance)
                                  (format nil "~A"
                                          (gir:gir-class-of i)))
        do (cond
             ((equalp item-class-string "#O<MenuItem>")
              (gio:menu-append-item submenu i))
             ((and (null item-class-string)
                   (consp i)
                   (eq :submenu (first i)))
              (gio:menu-append-submenu submenu (second i) (third i)))
             (T (error "unexpected item-class-string or option ~S" item-class-string)))))

;; ================================= MENU code ends here =======================

(defun menu-bar-menu (app)
  (let ((menu (gio:make-menu)))
    (build-menu
     menu
     (prepare-submenu
      "Game"
      (prepare-section
       nil
       (build-items
        (prepare-item-bool app menu "Dark mode" "dark-mode" nil)))
      (prepare-submenu
       "New Game"
       (prepare-section
        nil
        (progn
          (prepare-radio-action app "new-game-size" "SMALL")
          (build-items
           (prepare-item-radio app menu "Small 8x8"    "new-game-size" "SMALL")
           (prepare-item-radio app menu "Medium 16x16" "new-game-size" "MEDIUM")
           (prepare-item-radio app menu "Large 32x32"  "new-game-size" "LARGE"))))
       (prepare-section
        nil
        (build-items
         (prepare-item-simple app menu "Nothing" "nothing"))))
      (prepare-section
       nil
       (build-items
        (prepare-item-simple app menu "Quit" "quit"))))
     (prepare-submenu
      "Help"
      (prepare-section
       nil
       (build-items
        (prepare-item-simple app menu "Help" "help")
        (prepare-item-simple app menu "Tutorial" "tutorial" :disabled T)))
      (prepare-section
       nil
       (build-items
        (prepare-item-simple app menu "About" "about")))))

    (values menu)))

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
                     (gtk4:window-title window) "Better Menu"
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

(defun window ()
  (let ((app (gtk:make-application :application-id "org.bigos.gtk4-example.better-menu"
                                   :flags gio:+application-flags-flags-none+)))
    (window-activation app)

    (let ((status (gtk:application-run app nil)))
      (setf *canvas* nil)
      (gobj:object-unref app)
      status)))

(defun main ()
  (init-model)
  (window))
