(in-package :dhs-sequences)

(defclass generic-map (container)
  ()
  (:documentation "Superclass of all maps"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default hash map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hash-map (generic-map)
  ((map :type hash-table
        :reader hash-map/map))
  (:documentation "Wrapper for the CL standard hash-table type"))

(defmethod initialize-instance :after ((obj hash-map) &key (test 'eql))
  (setf (slot-value obj 'map) (make-hash-table :test test)))

(defun make-hash-map (&key (test 'eql))
  (make-instance 'hash-map :test test))

(defgeneric hash-get (map key))
(defgeneric (setf hash-get) (value map key))
(defgeneric hash-remove (map key))
(defgeneric hash-get-or-update (map key update-fn))
(defgeneric hash-iterator (map &key content))

(defmethod content-length ((map hash-map))
  (hash-table-size (hash-map/map map)))

(defmethod empty-p ((map hash-map))
  (zerop (hash-table-size (hash-map/map map))))

(defmethod hash-get ((map hash-map) key)
  (gethash key (hash-map/map map)))

(defmethod (setf hash-get) (value (map hash-map) key)
  (setf (gethash key (hash-map/map map)) value))

(defmethod hash-remove ((map hash-map) key)
  (remhash key (hash-map/map map)))

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

(defmethod hash-iterator ((map hash-map) &key (content :key))
  (let ((keys (ecase content
                (:key (loop
                         for key being each hash-key in (hash-map/map map)
                         collect key))
                (:value (loop
                           for value being each hash-value in (hash-map/map map)
                           collect value))
                (:both (loop
                          for key being each hash-key in (hash-map/map map) using (hash-value value)
                          collect (cons key value))))))
    (let ((element keys))
      (lambda ()
        (let ((key (car element)))
          (setf element (cdr element))
          key)))))

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

(defmethod hash-remove :around ((map blocking-hash-map) key)
  (with-locked-instance map
    (call-next-method)))

(defmethod hash-get-or-update ((map blocking-hash-map) key update-fn)
  (with-locked-instance map
    (call-next-method)))
