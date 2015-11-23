(asdf:defsystem #:containers-tests
  :description "Tests for dhs-sequences"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on (:fiveam
               :containers
               :acm-random)
  :components ((:module tests
                        :serial t
                        :components ((:file "package")
                                     (:file "common-tests")
                                     (:file "queue-tests")
                                     (:file "rbtree-tests")
                                     (:file "rbtree-special-test-data")))))
