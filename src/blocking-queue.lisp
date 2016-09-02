(in-package :receptacle)

(defclass generic-sequence ()
  ()
  (:documentation "Generic ordered sequence"))

(define-condition sequence-empty (error)
  ()
  (:documentation "Error that is raised if an attempt is made to
access pop an element from an empty sequence."))

#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (push :log-queue *features*)
  (push :debug-queue *features*))

(defclass queue (generic-sequence)
  ((content :type (vector t *)
            :initform (make-array 32 :adjustable t)
            :accessor queue/content)
   (head    :type (integer 0)
            :initform 0
            :accessor queue/head)
   (tail    :type (integer 0)
            :initform 0
            :accessor queue/tail))
  (:documentation "Queue that supports insertion and removal from both the tail and head"))

(defmethod print-object ((obj queue) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (with-slots (content head tail) obj
      (format stream "SIZE ~a" (mod (- tail head) (array-dimension content 0))))))

(defmethod content-length ((queue queue))
  (with-slots (head tail content) queue
    (mod (- tail head) (array-dimension content 0))))

(defmethod empty-p ((queue queue))
  (with-slots (head tail) queue
    (= head tail)))

(defmethod delete-all ((queue queue))
  (loop
     with content = (queue/content queue)
     with length = (array-dimension content 0)
     for i from 0 below length
     do (setf (aref content i) nil))
  (setf (queue/head queue) 0)
  (setf (queue/tail queue) 0))

(defun make-queue ()
  "Create a new instance of a queue."
  (make-instance 'queue))

(defmethod queue-push ((queue queue) element)
  (with-slots (content head tail) queue
    (let ((size (array-dimension content 0)))
      (when (= (mod (- tail head) size) (1- size))
        (let* ((new-size (* size 2))
               (new-head (- new-size (- size head))))
          (adjust-array content new-size)
          (when (< tail head)
            (loop
               for src from head below size
               for dest from new-head
               do (setf (aref content dest) (aref content src)))
            (setq head new-head))))
      (setf (aref content tail) element)
      (setq tail (mod (1+ tail) (array-dimension content 0)))
      element)))

(defmethod queue-pop ((queue queue) &key (if-empty nil if-empty-set-p))
  (with-slots (content head tail) queue
    (if (= head tail)
        (if if-empty-set-p
          if-empty
          (error 'sequence-empty))
        (let ((result (aref content head)))
          (setf (aref content head) nil)
          (setq head (mod (1+ head) (array-dimension content 0)))
          result))))

(defgeneric queue-pop-wait (queue &key timeout)
  (:documentation "Attempts to pop one element off QUEUE. If the queue
is empty, wait until an element is added."))

(defclass blocking-queue (queue lockable-instance)
  ()
  (:documentation "A thread-safe version of QUEUE that allows waiting
for elements to be added to it."))

(defun make-blocking-queue (&key name)
  "Create a new instance of a blocking queue."
  (make-instance 'blocking-queue :lockable-instance-name name))

(defmethod empty-p ((queue blocking-queue))
  (with-locked-instance queue
    (call-next-method)))

(defmethod delete-all ((queue blocking-queue))
  (with-locked-instance queue
    (call-next-method)))

(defmethod queue-push ((queue blocking-queue) element)
  (declare (ignore element))
  (with-locked-instance queue
    (let ((result (call-next-method)))
      (bordeaux-threads:condition-notify (lockable-instance/cond-variable queue))
      result)))

(defmethod queue-pop ((queue blocking-queue) &rest rest)
  (declare (ignore rest))
  (with-locked-instance queue
    (call-next-method)))

(defun current-time ()
  (let ((now (local-time:now)))
    (+ (local-time:timestamp-to-unix now)
       (/ (local-time:nsec-of now) 1000000000))))

(defun %queue-pop-wait (queue timeout)
  (with-locked-instance queue
    (if (empty-p queue)
        (when (let ((condition (lockable-instance/cond-variable queue))
                    (lock (lockable-instance/lock queue)))
                #+sbcl
                (sb-thread:condition-wait condition lock :timeout timeout)
                #+abcl
                (progn
                  (threads:synchronized-on condition
                    (bordeaux-threads:release-lock lock)
                    (apply #'threads:object-wait condition (if timeout (list timeout))))
                  (bordeaux-threads:acquire-lock lock)
                  t)
                #+ccl
                (progn
                  (ccl:release-lock lock)
                  (unwind-protect
                       (if timeout
                           (ccl:timed-wait-on-semaphore condition timeout)
                           (ccl:wait-on-semaphore condition))
                    (ccl:grab-lock lock)))
                #+clisp
                (mt:exemption-wait condition lock :timeout timeout)
                #-(or sbcl abcl ccl clisp)
                (progn
                  (bordeaux-threads:condition-wait condition lock)
                  t))
          (unless (empty-p queue)
            (queue-pop queue)))
        ;; ELSE: We have an element on the queue
        (queue-pop queue))))

(defmethod queue-pop-wait ((queue blocking-queue) &key timeout)
  #-(or sbcl abcl ccl clisp)
  (when timeout
    (error "Timeout is not supported on this CL implementation"))
  (check-type timeout (or null number))
  (if timeout
      (loop
         ;; wait-time-epsilon indicates the minimum amount of time to wait
         with wait-time-epsilon = 1/1000
         with start = (current-time)
         with now = start
         with cutoff = (+ start (rationalize timeout))
         while (< now cutoff)
         do (let ((result (%queue-pop-wait queue (max (- cutoff now) wait-time-epsilon))))
              (when result
                (return result))
              (setq now (current-time)))
         finally (return nil))
      ;; ELSE: No timeout
      (loop
         do (let ((result (%queue-pop-wait queue nil)))
              (when result
                (return result))))))
