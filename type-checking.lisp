(in-package #:clos-sweeper)

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
