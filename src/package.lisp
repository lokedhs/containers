(defpackage :receptacle
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
           #:with-locked-instance
           #:hash-iterator
           #:hash-remove
           #:cas
           #:make-cas-wrapper
           #:cas-wrapper/value
           #:cas-wrapper
           #:content-length
           #:make-queue
           #:call-with-cas
           #:with-cas-update
           #:queue
           #:hash-keys
           #:delete-all
           #:container
           #:tree-insert
           #:tree-find-node
           #:tree-delete-node
           #:tree-delete-element
           #:tree-first-node
           #:tree-first-element
           #:node-element
           #:tree-previous
           #:tree-next
           #:tree-find-element
           #:make-container-iterator
           #:iterator/get-next-element
           #:container-nth
           #:tree))

(defpackage :receptacle.red-black-tree
    (:use :cl :receptacle)
    (:export #:red-black-tree))

(defpackage :receptacle.flexichain
    (:use :cl)
    (:export))
