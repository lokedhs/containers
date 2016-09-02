(in-package :receptacle)

(defclass lockable-instance ()
  ((lock          :type t
                  :reader lockable-instance/lock)
   (cond-variable :type t
                  :reader lockable-instance/cond-variable))
  (:documentation "Sequence that supports blocking on empty queues"))

(defmethod initialize-instance :after ((obj lockable-instance) &key lockable-instance-name)
  (setf (slot-value obj 'lock) (bordeaux-threads:make-recursive-lock lockable-instance-name))
  (setf (slot-value obj 'cond-variable) (bordeaux-threads:make-condition-variable :name lockable-instance-name)))

(defmacro with-locked-instance (obj &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lockable-instance/lock ,obj))
     ,@body))

(defmacro with-locked-instance-maybe (obj &body body)
  (let ((obj-sym (gensym "OBJ-"))
        (body-sym (gensym "BODY-")))
    `(let ((,obj-sym ,obj))
       (flet ((,body-sym () ,@body))
         (if (typep ,obj-sym 'lockable-instance)
             (with-locked-instance ,obj-sym (,body-sym))
             (,body-sym))))))

(defmacro with-disabled-interrupts (&body body)
  `(progn
     #+sbcl (sb-sys:without-interrupts ,@body)
     #-sbcl (progn ,@body)))
