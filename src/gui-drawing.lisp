(in-package #:clos-sweeper)

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
                                (format nil "~Acairo-simulate-drawing~A.png"
                                        (uiop:temporary-directory)
                                        (get-internal-run-time)))))

;;; ============================ view ==========================================
(defun draw-objects (w h)               ; view
  ;; (format t ">>>>>>>> in draw-objects ~S " cairo:*context*)
  (let ((cnt (format nil "~A" (selection *model*)))
        (tpx 0)
        (tpy 0))
    (if (dark-mode *model*)
        (cairo:set-source-rgb  0.1 0.1 0.1)
        (cairo:set-source-rgb  1 1 1))
    (cairo:paint)

    (render (grid *model*))


    (cairo:select-font-face "Ubuntu Mono" :normal :bold)
    (cairo:set-font-size 20)
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents cnt)
      (declare (ignore xb yb width height))
    )
    (setf tpx 6
          tpy 15)
    (if (dark-mode *model*)
        (cairo:set-source-rgb 1 1 1)
        (cairo:set-source-rgb 0 0 0))
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
