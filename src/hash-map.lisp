(in-package :containers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default hash map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hash-map ()
  ((map :type hash-table
        :reader hash-map/map))
  (:documentation "Wrapper for the CL standard hash-table type"))

(defmethod initialize-instance :after ((obj hash-map) &key (test 'eql))
  (setf (slot-value obj 'map) (make-hash-table :test test)))

(defun make-hash-map (&key (test 'eql))
  (make-instance 'hash-map :test test))

(defgeneric hash-get (map key))
(defgeneric (setf hash-get) (value map key))
(defgeneric hash-get-or-update (map key update-fn))

(defmethod hash-get ((map hash-map) key)
  (gethash key (hash-map/map map)))

(defmethod (setf hash-get) (value (map hash-map) key)
  (setf (gethash key (hash-map/map map)) value))

(defmethod hash-get-or-update ((map hash-map) key update-fn)
  (check-type update-fn function)
  (multiple-value-bind (value exist-p)
      (hash-get map key)
    (if exist-p
        value
        (setf (hash-get map key) (funcall update-fn)))))

(defmacro with-hash-get-or-update (map key &body body)
  (alexandria:once-only (map key)
    (let ((body-sym (gensym "BODY-")))
      `(flet ((,body-sym () ,@body))
         (hash-get-or-update ,map ,key #',body-sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thread-safe hash map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass blocking-hash-map (hash-map lockable-instance)
  ()
  (:documentation "Blocking version of the plain hash-map"))

(defun make-blocking-hash-map (&key (test 'eql) name)
  (make-instance 'blocking-hash-map :test test :lockable-instance-name name))

(defmethod hash-get :around ((map blocking-hash-map) key)
  (with-locked-instance map
    (call-next-method)))

(defmethod (setf hash-get) :around (value (map blocking-hash-map) key)
  (with-locked-instance map
    (call-next-method)))

(defmethod hash-get-or-update ((map blocking-hash-map) key update-fn)
  (with-locked-instance map
    (call-next-method)))
