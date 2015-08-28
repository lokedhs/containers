(in-package :dhs-sequence-tests)

(declaim (optimize (safety 3) (speed 0) (debug 3)))

(defclass cas-test-element ()
  ((index :type integer
          :initarg :index
          :reader cas-test-element/index)
   (val   :type cas-wrapper
          :initform (make-cas-wrapper nil)
          :reader cas-test-element/val)))

(defun make-source-data (n)
  (loop
     repeat n
     for i from 0
     collect (make-instance 'cas-test-element :index i)))

(fiveam:test cas-single-thread-test
  (let* ((iterations 10000)
         (source-data (make-source-data iterations))
         (q (make-blocking-queue))
         (results (make-blocking-queue)))
    (labels ((load-values ()
               (loop
                  for obj = (queue-pop-wait q :timeout 10)
                  while (and obj (not (eq obj :stop)))
                  do (queue-push results (list obj (cas-wrapper/value (cas-test-element/val obj)))))))
      (let ((consumer-thread (bordeaux-threads:make-thread #'load-values)))
        (loop
           for obj in source-data
           do (progn
                (let ((res (cas (cas-test-element/val obj) nil t)))
                  (fiveam:is (null res))
                  (queue-push q obj))))
        (queue-push q :stop)
        (bordeaux-threads:join-thread consumer-thread)
        (loop
           repeat iterations
           for i from 0
           for obj = (queue-pop results :if-empty nil)
           unless obj
           do (error "Not enough elements in queue")
           do (progn
                (fiveam:is (eql i (cas-test-element/index (first obj))))
                (fiveam:is (eq t (second obj)))))))))

(fiveam:test cas-n-threads-test
  (let ((num-threads 10)
        (num-iterations 1000000)
        (wrapper (make-cas-wrapper 0))
        (results (make-blocking-queue)))
    (labels ((increment ()
               (with-cas-update (value wrapper)
                 (1+ value))))
      (let ((threads (loop
                        repeat num-threads
                        collect (bordeaux-threads:make-thread (lambda ()
                                                                (loop
                                                                   repeat num-iterations
                                                                   do (let* ((prev (cas-wrapper/value wrapper))
                                                                             (res (increment))
                                                                             (current (cas-wrapper/value wrapper)))
                                                                        (unless (and (<= prev res)
                                                                                     (<= res current))
                                                                          (queue-push results (list prev res current))))))))))
        (dolist (thread threads)
          (bordeaux-threads:join-thread thread))
        (fiveam:is (= (* num-threads num-iterations) (cas-wrapper/value wrapper)))
        (fiveam:is (zerop (content-length results)))))))
