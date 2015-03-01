(in-package :dhs-sequences)

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
            :accessor queue/tail)
   #+log-queue(log :type list
                   :initform nil))
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

(defun make-queue ()
  "Create a new instance of a queue."
  (make-instance 'queue))

(defgeneric queue-push (queue element)
  (:documentation "Push ELEMENT to the tail of QUEUE"))

(defgeneric queue-pop (queue &key if-empty)
  (:documentation "Pop the element from the head of QUEUE"))

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

(defmethod queue-push ((queue blocking-queue) element)
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
                #-(or sbcl abcl)
                (progn
                  (bordeaux-threads:condition-wait condition lock)
                  t))
          (unless (empty-p queue)
            (queue-pop queue)))
        ;; ELSE: We have an element on the queue
        (queue-pop queue))))

(defmethod queue-pop-wait ((queue blocking-queue) &key timeout)
  #-(or sbcl abcl) (when timeout
                     (error "Timeout is only supported on SBCL and ABCL"))
  (check-type timeout (or null number))
  (if timeout
      (loop
         with start = (current-time)
         with now = start
         with cutoff = (+ start (rationalize timeout))
         do (let ((result (%queue-pop-wait queue (- cutoff now))))
              (when result
                (return result))
              (setq now (current-time)))
         while (< now cutoff)
         finally (return nil))
      ;; ELSE: No timeout
      (loop
         do (let ((result (%queue-pop-wait queue nil)))
              (when result
                (return result))))))

#+debug-queue
(defmethod queue-push :around ((q blocking-queue) element)
  (with-locked-instance q
    (let* ((size-before (content-length q))
           (result (call-next-method))
           (size-after (content-length q)))
      (when (/= size-before (1- size-after))
        (log:error "Inconsistent result from push"))
      #+log-queue(push (list 'push size-before size-after element result) (slot-value q 'log))
      result)))

#+debug-queue
(defmethod queue-pop :around ((q blocking-queue) &key (if-empty nil if-empty-set-p))
  (declare (ignore if-empty))
  (with-locked-instance q
    (let* ((size-before (content-length q))
           (result (call-next-method))
           (size-after (content-length q)))
      (when (and (or (not if-empty-set-p)
                     (not (zerop size-before)))
                 (/= size-before (1+ size-after)))
        (log:error "Inconsistent result from pop, size-before=~s, size-after=~s, if-empty-set-p=~s"
                   size-before size-after if-empty-set-p))
      #+log-queue(push (list 'pop size-before size-after result) (slot-value q 'log))
      result)))
