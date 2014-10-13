(in-package :dhs-sequences)

(defclass task-queue ()
  ((name        :type string
                :initarg :name
                :initform "Task queue"
                :reader task-queue/name)
   (queue       :type blocking-queue
                :reader task-queue/queue)
   (max-workers :type (integer 1)
                :initarg :max-workers
                :reader task-queue/max-workers)))

(defmethod initialize-instance :after ((obj task-queue) &key queue)
  (setf (slot-value obj 'queue) (or queue (make-blocking-queue :name (task-queue/name obj)))))

(defgeneric queue-job (queue job))

(defmethod queue-job ((queue task-queue) job)
  (check-type job function)
  )
