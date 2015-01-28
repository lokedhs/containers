(asdf:defsystem #:containers-tests
  :description "Tests for dhs-sequences"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on (:fiveam
               :containers)
  :components ((:module tests
                        :serial t
                        :components ((:file "package")
                                     (:file "queue-tests")))))
