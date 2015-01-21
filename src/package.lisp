(defpackage :dhs-sequences
  (:use :cl)
  (:documentation "Container library")
  (:export #:queue-push
           #:empty-p
           #:sequence-empty
           #:queue-pop
           #:queue-pop-wait
           #:make-blocking-queue
           #:atomic/value
           #:make-atomic-variable
           #:atomic-variable
           #:with-atomic-variable
           #:hash-map
           #:make-hash-map
           #:hash-get
           #:make-blocking-hash-map
           #:blocking-hash-map
           #:with-hash-get-or-update
           #:hash-get-or-update
           #:blocking-queue
           #:with-disabled-interrupts
           #:queue-start-workers
           #:queue-job
           #:task-queue
           #:with-locked-instance
           #:hash-iterator
           #:hash-remove
           #:cas
           #:make-cas-wrapper
           #:cas-wrapper/value))
