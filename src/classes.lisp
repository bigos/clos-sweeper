(in-package #:clos-sweeper)

;;; ====================== classes =============================================
(defclass/std model (set-unbound-slots-mixin)
  ((selection :type string)
   (width  :documentation "Canvas width")
   (height :documentation "Canvas height")
   (mouse-x)
   (mouse-y)
   (button-1)
   (button-2)
   (button-3)
   (grid))
  (:metaclass checked-class))

(defun init-model (&optional width height (grid-size 8))
  (setf *model* (make-instance 'model
                               :selection "start uncovering mines"
                               :width width
                               :height height))
  (setf (grid *model*) (build-grid grid-size))
  *model*)

(defclass/std box (rel)
  ((top-left-x)
   (top-left-y)
   (bottom-right-x)
   (bottom-right-y)
   (state))) ; covered-empty=nil, covered-mine, empty, flagged-mine, exploded-mine

(defclass/std grid (box)
  ((grid-size)))

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

(defun build-grid (&optional (size 8))
  (let* ((offset-x 50)
         (offset-y 20)
         (grid-size (if (member size '(8 16 32))
                        size
                        (error "Unexpected size ~S" size)))
         (dist (* 0.8  (min (or (width *model*)
                                20)
                            (or (height *model*)
                                20))))
         (tile-size (* 0.70 dist))
         (mines-count  (floor (/ (expt grid-size 2)
                                 4)))
         (parent (make-instance 'grid
                                :id :GRID
                                :top-left-x offset-x
                                :top-left-y offset-x
                                :bottom-right-x  (* dist grid-size)
                                :bottom-right-y  (* dist grid-size)
                                :grid-size grid-size)))
                                        ;that still shows, we need better way for different boxes
    (loop for r from 0 below grid-size do
      (loop for c from 0 below grid-size do
        (let* ((k (list :id r c))
               (box (build-box 'box
                               k
                               (+ offset-x (* r dist))
                               (+ offset-y (* c dist))
                               tile-size tile-size)))
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

;;; for testing
(defmethod middle ((box box))
  (list
   (+ (top-left-x box)
      (/ (width box) 2))
   (+ (top-left-y box)
      (/ (height box) 2))))

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
  (cairo:select-font-face "Ubuntu Mono" :normal :bold)
  (cairo:set-font-size 20)
  (let* ((text-height 20)
         (minsize (min (width *model*)
                       (- (height *model*)
                          (* 1.5 text-height))))
         (dist (* 0.90 (/ minsize (grid-size grid))))
         (tile-size (* 0.75 dist))
         (vertical-center (/ (width *model*) 2))
         (offset-x  (- vertical-center  (* (/ (grid-size grid) 2) dist) ))
         (offset-y 25))
    (warn "text height is ~a" text-height)
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
