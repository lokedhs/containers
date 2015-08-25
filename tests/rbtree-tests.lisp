(in-package :dhs-sequence-tests)

(declaim (optimize (safety 3) (speed 0) (debug 3)))

(defun make-value-producer ()
  (let ((i 0))
    (lambda ()
      (format nil "~r" (incf i)))))

(fiveam:test rbtree-insert-elements-test
  (let ((values (loop
                   with p = (make-value-producer)
                   repeat 10000
                   collect (funcall p)))
        (q (make-instance 'dhs-sequences.red-black-tree:red-black-tree)))
    (loop
       for x in values
       do (dhs-sequences.red-black-tree::tree-insert q x))
    (let ((sorted (sort values #'string<))
          (from-tree (loop
                        for x = (dhs-sequences.red-black-tree::tree-first-node q) then (dhs-sequences.red-black-tree::tree-successor q x)
                        until (eq x (dhs-sequences.red-black-tree::red-black-tree/empty-node q))
                        collect (dhs-sequences.red-black-tree::node/value x))))
      (fiveam:is (equal sorted from-tree)))))
