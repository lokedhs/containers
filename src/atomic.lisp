(in-package :dhs-sequences)

(defclass atomic-variable ()
  ((value :type t
          :initarg :value
          :initform (error "~s not supplied when creating ~s" :value 'atomic-variable)
          :accessor atomic-variable/value)
   (lock  :type t
          :initform (bordeaux-threads:make-lock "Atomic variable lock")
          :reader atomic-variable/lock))
  (:documentation "Class that holds a single value. The value can be
get or set atomically."))

(defmethod print-object ((obj atomic-variable) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~s" (if (slot-boundp obj 'value)
                            (atomic-variable/value obj)
                            :not-bound))))

(defun make-atomic-variable (value)
  (make-instance 'atomic-variable :value value))

(defgeneric (setf atomic/value) (value variable))

(defmethod (setf atomic/value) (value (variable atomic-variable))
  (bordeaux-threads:with-lock-held ((atomic-variable/lock variable))
    (setf (atomic-variable/value variable) value)))

(defgeneric atomic/value (variable))

(defmethod atomic/value ((variable atomic-variable))
  (bordeaux-threads:with-lock-held ((atomic-variable/lock variable))
    (atomic-variable/value variable)))

(defmacro with-atomic-variable ((sym variable) &body body)
  (alexandria:with-gensyms (variable-sym)
    `(let ((,variable-sym ,variable))
       (symbol-macrolet ((,sym (atomic-variable/value ,variable-sym)))
         (bordeaux-threads:with-lock-held ((atomic-variable/lock ,variable-sym))
           (with-disabled-interrupts
             ,@body))))))

;;;
;;;  CAS implementation
;;;

(defclass cas-wrapper ()
  ((value :type t
          :initarg :value)
   #-sbcl
   (lock  :type t
          :initform (bordeaux-threads:make-lock))))

(defun make-cas-wrapper (value)
  (make-instance 'cas-wrapper :value value))

(defun cas (wrapper old-value new-value)
  #+sbcl
  (sb-ext:cas (slot-value wrapper 'value) old-value new-value)
  #-sbcl
  (bordeaux-threads:with-lock-held ((slot-value wrapper 'lock))
    (let ((v (slot-value wrapper 'value)))
      (when (eq v old-value)
        (setf (slot-value wrapper 'value) new-value))
      v)))

(defun cas-wrapper/value (wrapper)
  (slot-value wrapper 'value))

(defun call-with-cas (wrapper fn)
  (loop
     for old = (cas-wrapper/value wrapper)
     for v = (funcall fn old)
     for result = (cas wrapper old v)
     when (eq old result)
     return v))
