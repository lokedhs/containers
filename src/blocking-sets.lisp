(in-package :containers)

(defclass generic-set ()
  ()
  (:documentation "Generic unordered sets"))

(defgeneric contains-element-p (set object)
  (:documentation "Returns non-NIL if SET contains OBJECT"))

(defclass hash-set (generic-set)
  ((content :type hash-map
            :reader hash-set/content))
  (:documentation "Set that is backed by the hash-map class."))

(defmethod initialize-instance :after ((obj hash-set) &key (test 'eql))
  (setf (slot-value obj 'content) (make-hash-map :test test)))

(defun make-hash-set (&key (test 'eql))
  (make-instance 'hash-set :test test))
