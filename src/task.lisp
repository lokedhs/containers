(in-package :dhs-sequences)

(defclass task-queue (lockable-instance)
  ((name         :type string
                 :initarg :name
                 :initform "Task queue"
                 :reader task-queue/name)
   (queue        :type blocking-queue
                 :reader task-queue/queue)
   (max-workers  :type (integer 1)
                 :initarg :max-workers
                 :initform 1
                 :reader task-queue/max-workers)
   (workers      :type list
                 :initform nil
                 :accessor task-queue/workers)))

(defmethod initialize-instance :after ((obj task-queue) &key)
  (setf (slot-value obj 'queue)
        (make-blocking-queue :name (task-queue/name obj))))

(defgeneric queue-job (queue job))

(defmethod queue-job ((queue task-queue) job)
  (check-type job function)
  (queue-push (task-queue/queue queue) job))

(defgeneric queue-start-workers (queue))

(defun task-queue-worker-main (queue)
  (loop
     for job = (queue-pop-wait (task-queue/queue queue))
     do (if (functionp job)
            (funcall job)
            (warn "Job is not a function: ~s" job))))

(defmethod queue-start-workers ((queue task-queue))
  (bordeaux-threads:with-lock-held ((lockable-instance/lock queue))
    (when (task-queue/workers queue)
      (error "Task queue has already been started"))
    (let ((threads (loop
                      repeat (task-queue/max-workers queue)
                      for i from 0
                      collect (bordeaux-threads:make-thread #'(lambda () (task-queue-worker-main queue))
                                                            :name (format nil "Task queue ~a. Thread ~a"
                                                                          (task-queue/name queue) i)))))
      (setf (task-queue/workers queue) threads))))
