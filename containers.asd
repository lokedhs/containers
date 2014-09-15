(asdf:defsystem #:containers
    :description "Container library"
    :author "Elias Martenson <lokedhs@gmail.com>"
    :license "BSD"
    :serial t
    :depends-on (:alexandria
                 :bordeaux-threads)
    :components ((:module src
                          :serial t
                          :components ((:file "package")
                                       (:file "locks")
                                       (:file "generic")
                                       (:file "blocking-queue")
                                       (:file "hash-map")
                                       (:file "blocking-sets")
                                       (:file "atomic")))))
