(in-package :dhs-sequences)

(defclass standard-atomic-variable ()
  ((value :type t
          :initarg :value
          :initform (error "~s not supplied when creating ~s" :value 'atomic-variable)
          :accessor standard-atomic-variable/value)
   (lock  :type t
          :initform (bordeaux-threads:make-lock "Atomic variable lock")
          :reader standard-atomic-variable/lock))
  (:documentation "Class that holds a single value. The value can be
get or set atomically."))

(defmethod print-object ((obj standard-atomic-variable) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~s" (if (slot-boundp obj 'value)
                            (standard-atomic-variable/value obj)
                            :not-bound))))

(defgeneric atomic/value (variable))
(defgeneric (setf atomic/value) (value variable))
(defgeneric call-with-atomic-variable (variable setter-fn))

(defun make-atomic-variable (value)
  (make-instance 'standard-atomic-variable :value value))

(defun make-cas-atomic-variable (value)
  #+sbcl (make-instance 'sbcl-atomic :value value)
  #-sbcl (make-atomic-variable value))

(defmethod (setf atomic/value) (value (variable standard-atomic-variable))
  (bordeaux-threads:with-recursive-lock-held ((standard-atomic-variable/lock variable))
    (setf (standard-atomic-variable/value variable) value)))

(defmethod atomic/value ((variable standard-atomic-variable))
  (bordeaux-threads:with-recursive-lock-held ((standard-atomic-variable/lock variable))
    (standard-atomic-variable/value variable)))

(defmethod call-with-atomic-variable ((variable standard-atomic-variable) setter-fn)
  (bordeaux-threads:with-recursive-lock-held ((standard-atomic-variable/lock variable))
    (with-disabled-interrupts
      (funcall setter-fn variable))))

(defmacro with-atomic-variable ((sym variable) &body body)
  (alexandria:with-gensyms (variable-sym)
    `(call-with-atomic-variable ,variable
                                (lambda (,variable-sym)
                                  (symbol-macrolet ((,sym (atomic/value ,variable-sym)))
                                    ,@body)))))

;;;
;;;  SBCL implementation using CAS
;;;

#+sbcl
(progn
  (defclass sbcl-atomic ()
    ((value :type t
            :initarg :value
            :initform (error "~s not supplied when creating ~s" :value 'atomic-variable)
            :accessor sbcl-atomic/value)))

  (defmethod print-object ((obj sbcl-atomic) stream)
    (print-unreadable-object (obj stream :type t :identity nil)
      (format stream "~s" (if (slot-boundp obj 'value)
                              (sbcl-atomic/value obj)
                              :not-bound))))

  (defmethod atomic/value ((variable sbcl-atomic))
    (sbcl-atomic/value variable))

  (defmethod (setf atomic/value) (value (variable sbcl-atomic))
    (loop
       for old = (sbcl-atomic/value variable)
       while (not (eq value old))
       do (sb-ext:cas (slot-value variable 'value) old value)))

  (defmethod call-with-atomic-variable ((variable sbcl-atomic) setter-fn)
    (funcall setter-fn variable)))
