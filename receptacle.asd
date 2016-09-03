(asdf:defsystem #:receptacle
    :description "Container library"
    :author "Elias Martenson <lokedhs@gmail.com>"
    :license "BSD"
    :serial t
    :depends-on (:alexandria
                 :bordeaux-threads
                 :local-time
                 :flexichain)
    :components ((:module src
                          :serial t
                          :components ((:file "package")
                                       (:file "locks")
                                       (:file "generic")
                                       (:file "default-types")
                                       (:file "blocking-queue")
                                       (:file "list-queue")
                                       (:file "hash-map")
                                       (:file "blocking-sets")
                                       (:file "atomic")
                                       (:file "tree")
                                       (:file "sorted-list")
                                       (:file "rbtree")
                                       (:file "flexichain")))))
