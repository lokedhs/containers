(in-package :receptacle)

;;;
;;;  Lisp lists
;;;

(defmethod content-length ((container list))
  (length container))

(defmethod empty-p ((container list))
  (null container))

(defmethod container-nth ((container list) index)
  (nth index container))

(defmethod insert-at-position ((container list) index element)
  (when (zerop index)
    (error "Insertion at position 0 not supported for plain lists"))
  (let ((rest (nthcdr (1- index) container)))
    (setf (cdr rest) (cons element (cdr rest)))))

(defmethod remove-at-position ((container list) index)
  (when (zerop index)
    (error "Can't remove the first element in Lisp lists"))
  (let ((rest (nthcdr (1- index) container)))
    (setf (cdr rest) (cddr rest))))

(defmethod (setf container-nth) (element (container list) index)
  (setf (nth index container) element))

(defclass std-list-iterator ()
  ((element :type t
            :initarg :element
            :accessor std-list-iterator/element)))

(defmethod make-container-iterator ((container list))
  (make-instance 'std-list-iterator :element container))

(defmethod iterator/get-next-element ((iterator std-list-iterator))
  (let ((element (std-list-iterator/element iterator)))
    (cond (element
           (setf (std-list-iterator/element iterator) (cdr element))
           (values (car element) nil))
          (t
           (values nil t)))))

;;;
;;; Arrays
;;;

(defmethod content-length ((container array))
  (array-total-size container))

(defmethod empty-p ((container array))
  (zerop (array-total-size container)))

(defmethod container-nth ((container array) index)
  (row-major-aref container index))

(defmethod insert-at-position ((container vector) index element)
  (unless (adjustable-array-p container)
    (error "Insert on arrays is only supported for adjustable arrays"))
  (let ((length (array-dimension container 0)))
    (adjust-array container (1+ length))
    (loop
      for i from (1- length) downto index
      do (setf (aref container (1+ i)) (aref container i)))
    (setf (aref container index) element)))

(defmethod remove-at-position ((container vector) index)
  (unless (adjustable-array-p container)
    (error "Remove on arrays is only supported for adjustable arrays"))
  (let ((length (array-dimension container 0)))
    (loop
      for i from (1+ index) below length
      do (setf (aref container (1- i)) (aref container i)))
    (adjust-array container (1- length))))

(defmethod (setf container-nth) (element (container vector) index)
  (setf (aref container index) element))

(defclass std-array-iterator ()
  ((array :type array
          :initarg :array
          :reader std-array-iterator/array)
   (pos   :type (integer 0)
          :initform 0
          :accessor std-array-iterator/pos)))

(defmethod make-container-iterator ((container array))
  (make-instance 'std-array-iterator :array container))

(defmethod iterator/get-next-element ((iterator std-array-iterator))
  (let ((array (std-array-iterator/array iterator))
        (pos (std-array-iterator/pos iterator)))
    (cond ((< pos (array-total-size array))
           (incf (std-array-iterator/pos iterator))
           (values (row-major-aref array pos) nil))
          (t
           (values nil t)))))

(defclass random-access-iterator ()
  ((container :type t
              :initarg :container
              :reader random-access-iterator/container)
   (pos       :type (integer 0)
              :initform 0
              :accessor random-access-iterator/pos))
  (:documentation "Implementation of an iterator that can be used for
containers which have an O(1) complexity when retrieving elements."))

(defmethod iterator/get-next-element ((iterator random-access-iterator))
  (let ((container (random-access-iterator/container iterator))
        (pos (random-access-iterator/pos iterator)))
    (cond ((< pos (content-length container))
           (incf (random-access-iterator/pos iterator))
           (values (container-nth container pos) nil))
          (t
           (values nil t)))))

;;;
;;;  General functions that probably should be somewhere else
;;;

(defmacro do-container ((sym container) &body body)
  (alexandria:with-gensyms (i value end-p)
    `(let ((,i (make-container-iterator ,container)))
       (loop
         while (multiple-value-bind (,value ,end-p)
                   (iterator/get-next-element ,i)
                 (if ,end-p
                     nil
                     (progn
                       (let ((,sym ,value))
                         ,@body)
                       t)))))))

(defclass test-fn-mixin ()
  ((test-equal-fn :type function
                  :initform #'string=
                  :initarg :test-equal
                  :reader test-fn-mixin/test-equal-fn)
   (test-fn       :type function
                  :initform #'string<
                  :initarg :test
                  :reader test-fn-mixin/test-fn)
   (key-fn        :type function
                  :initform #'identity
                  :initarg :key
                  :reader test-fn-mixin/key-fn)))
