(in-package :dhs-sequences)

(defclass list-queue (generic-sequence)
  ((head :type t
         :initarg :head
         :initform nil
         :accessor list-queue/head)
   (tail :type t
         :initarg :tail
         :initform nil
         :accessor list-queue/tail))
  (:documentation "Queue that is backed by a double-linked list. Each
node is a cons, with the value being the car and the previous node in
the cdr."))

(defun make-list-queue ()
  (make-instance 'list-queue))

(defmethod content-length ((queue list-queue))
  (length (list-queue/head queue)))

(defmethod queue-push ((queue list-queue) element)
  (with-slots (head tail) queue
    (let ((node (cons element nil)))
      (if head
          (setf (cdr head) node)
          (setf tail node))
      (setf head node))))

(defmethod queue-pop ((queue list-queue) &key (if-empty nil if-empty-set-p))
  (with-slots (head tail) queue
    (if tail
        (let ((node (car tail)))
          (setf tail (cdr tail))
          (if tail
              (setf (cdr tail) nil)
              (setf head nil))
          node)
        ;; ELSE: Queue is empty
        (if if-empty-set-p
            if-empty
            (error 'sequence-empty)))))
