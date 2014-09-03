(defpackage :containers
  (:use :cl)
  (:documentation "Container library")
  (:export #:queue-push
           #:seq-empty-p
           #:sequence-empty
           #:queue-pop
           #:queue-pop-wait
           #:make-blocking-queue))
