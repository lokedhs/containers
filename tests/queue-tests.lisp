(in-package :dhs-sequence-tests)

(declaim (optimize (speed 0) (debug 3)))

(defun check-with-tolerance (value expected tolerance)
  (fiveam:is (>= value (- expected tolerance)))
  (fiveam:is (<= value (+ expected tolerance))))

(defmacro define-queue-test ((name pop-fn-sym) &body body)
  (flet ((test-name (suffix)
           (intern (concatenate 'string (symbol-name name) "-" (symbol-name suffix)))))
    (alexandria:with-gensyms (queue-sym)
      `(progn
         (fiveam:test ,(test-name 'queue-pop)
           (macrolet ((,pop-fn-sym (,queue-sym) `(queue-pop ,,queue-sym)))
             ,@body))
         (fiveam:test ,(test-name 'queue-pop-wait)
           (macrolet ((,pop-fn-sym (,queue-sym) `(queue-pop-wait ,,queue-sym)))
             ,@body))
         (fiveam:test ,(test-name 'queue-pop-wait-with-timeout)
           (macrolet ((,pop-fn-sym (,queue-sym) `(queue-pop-wait ,,queue-sym :timeout 10)))
             ,@body))))))

(define-queue-test (test-simple-insert-remove queue-pop-fn)
  (let* ((q (make-blocking-queue))
         (n 10000)
         (orig (loop
                  for i from 0 below n
                  do (queue-push q i)
                  collect i)))
    (fiveam:is (not (empty-p q)))
    (fiveam:is (= (content-length q) n))
    (let ((content (loop
                      for i from 0 below n
                      collect (queue-pop-fn q))))
      (fiveam:is (equal content orig))
      (fiveam:is (empty-p q)))))


(define-queue-test (test-trailing-insert-remove queue-pop-fn)
  (let ((q (make-blocking-queue))
        (n 10000)
        (w 10)
        (i 0)
        (result nil))
    (loop
       repeat w
       do (queue-push q (incf i)))
    (fiveam:is (not (empty-p q)))
    (fiveam:is (= w (content-length q)))
    ;; Push and pop from the queue n times
    (loop
       repeat n
       do (queue-push q (incf i))
       do (push (queue-pop-fn q) result))
    (fiveam:is (not (empty-p q)))
    (fiveam:is (= w (content-length q)))
    (loop
       repeat w
       do (push (queue-pop-fn q) result))
    (fiveam:is (empty-p q))
    (fiveam:is (equal (loop for x from 1 to i collect x) (nreverse result)))))

(fiveam:test queue-with-timeout
  (let ((q (make-blocking-queue))
        (n 1000))
    (loop
       for i from 0 below n
       do (queue-push q i))
    (fiveam:is (= (content-length q) n))
    (let ((start-time (dhs-sequences::current-time))
          (result nil))
      (loop
         repeat (1+ n)
         do (push (queue-pop-wait q :timeout 0.4) result))
      (let* ((end-time (dhs-sequences::current-time))
             (delta (- end-time start-time)))
        (check-with-tolerance delta 0.4 0.1)
        (fiveam:is (equal (append (loop for i from 0 below n collect i)
                                  (list nil))
                          (nreverse result)))))))
