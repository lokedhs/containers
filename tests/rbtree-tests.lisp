(in-package :dhs-sequence-tests)

(declaim (optimize (safety 3) (speed 0) (debug 3)))

(defclass verifying-rbtree (receptacle.red-black-tree:red-black-tree)
  ((content :type list
            :initform nil
            :accessor verifying-rbtree/content)))

(defun check-verifying-rbtree (tree &optional info)
  (with-accessors ((content verifying-rbtree/content))
      tree
    (unless (= (receptacle:content-length tree)
               (length content))
      (error "Length mismatch. tree=~a, content=~a~@[, info=~s~]" (receptacle:content-length tree) (length content) info))
    (dolist (element content)
      (let ((node (receptacle:tree-find-node tree (funcall (receptacle.red-black-tree::red-black-tree/key-fn tree)
                                                              element))))
        (unless node
          (error "Element found in content but not in tree: ~s~@[, info=~s~]" element info))
        (unless (eq (receptacle:node-element node) element)
          (error "When looking up node for element ~s, got wrong node: ~s~@[, info=~s~]" element node info))))))

(defmethod receptacle:empty-p ((tree verifying-rbtree))
  (let ((result (call-next-method))
        (content-res (null (verifying-rbtree/content tree))))
    (unless (or (and result content-res)
                (and (not result) (not content-res)))
      (error "call to empty-p does not match"))
    result))

(defmethod receptacle:tree-delete-node ((tree verifying-rbtree) node)
  (with-accessors ((content verifying-rbtree/content))
      tree
    (let* ((element (receptacle:node-element node))
           (key (funcall (receptacle.red-black-tree::red-black-tree/key-fn tree) element)))
      (unless (member key content
                      :key (receptacle.red-black-tree::red-black-tree/key-fn tree)
                      :test (receptacle.red-black-tree::red-black-tree/test-equal-fn tree))
        (error "Attempt to delete node that is not in the tree: ~s" node))
      (let ((tree-info (format nil "Before: tree=~a, content=~a" (receptacle:content-length tree) (length content)))
            (result (call-next-method)))
        (setf content (delete key content
                              :key (receptacle.red-black-tree::red-black-tree/key-fn tree)
                              :test (receptacle.red-black-tree::red-black-tree/test-equal-fn tree)))
        (check-verifying-rbtree tree tree-info)
        result))))

(defmethod receptacle:tree-insert ((tree verifying-rbtree) element)
  (with-accessors ((content verifying-rbtree/content))
      tree
    (let ((result (call-next-method)))
      (when (member (funcall (receptacle.red-black-tree::red-black-tree/key-fn tree) element) content
                    :key (receptacle.red-black-tree::red-black-tree/key-fn tree)
                    :test (receptacle.red-black-tree::red-black-tree/test-equal-fn tree))
        (error "Attempt to add node that already exists in the tree: ~s" element))
      (pushnew element content
               :key (receptacle.red-black-tree::red-black-tree/key-fn tree)
               :test (receptacle.red-black-tree::red-black-tree/test-equal-fn tree))
      (check-verifying-rbtree tree)
      result)))

(defun make-value-producer ()
  (let ((i 0))
    (lambda ()
      (format nil "~r" (incf i)))))

(defun make-values-list (n)
  (loop
     with p = (make-value-producer)
     repeat n
     collect (funcall p)))

(defun tree-elements (q)
  (loop
     for x = (tree-first-node q) then (tree-next q x)
     while x
     collect (node-element x)))

(defun test-insert-n-elements (n)
  (let ((values (make-values-list n))
        (q (make-instance 'receptacle.red-black-tree:red-black-tree)))
    (dolist (v values)
      (tree-insert q v))
    (let ((sorted (sort values #'string<))
          (from-tree (tree-elements q)))
      (fiveam:is (equal sorted from-tree)))))

(fiveam:test rbtree-insert-elements-test
  (loop
     for i in '(1 2 3 4 5 6 7 8 9 10 100 1000 5000 10000)
     do (test-insert-n-elements i)))

(defun test-remove (n)
  (let ((values (make-values-list n))
        (q (make-instance 'receptacle.red-black-tree:red-black-tree)))
    (dolist (v values)
      (tree-insert q v))
    (let ((sorted (sort values #'string<)))
      (fiveam:is (equal sorted (tree-elements q)))
      (loop
         with z = sorted
         for x in values
         do (let ((node (tree-find-node q x)))
              (fiveam:is (not (null node)))
              (tree-delete-node q node)
              (setf z (remove x z :test #'string=))
              (fiveam:is (equal z (tree-elements q))))))))

(fiveam:test rbtree-remove-element-test
  (loop
     for i in '(1 2 3 4 5 6 7 8 9 10 100 1000 5000 10000)
     do (test-remove i)))

(fiveam:test rbtree-remove-all-test
  (let ((values (make-values-list 10000))
        (q (make-instance 'receptacle.red-black-tree:red-black-tree)))
    (dolist (v values)
      (tree-insert q v))
    (fiveam:is (= (length values) (receptacle:content-length q)))
    (receptacle:delete-all q)
    (fiveam:is (zerop (receptacle:content-length q)))))

(defun rbtree-perftest ()
  (let ((l (make-values-list 400000))
        (q (make-instance 'receptacle.red-black-tree:red-black-tree)))
    (time
     (progn
       (loop
          for v in l
          do (receptacle:tree-insert q v))
       (receptacle:content-length q)))))

(fiveam:test rbtree-timer-emulation
  (let ((tree (make-instance 'verifying-rbtree :key #'car :test #'< :test-equal #'=))
        (outstanding-nodes nil)
        (random (make-instance 'acm-random:acm-random)))
    ;;
    (labels ((remove-node (n)
               (progn
                 (receptacle:tree-delete-node tree n)
                 (unless (find (receptacle:node-element n) outstanding-nodes)
                   (error "Node missing from outstanding-nodes when deleting"))
                 (setf outstanding-nodes (remove (receptacle:node-element n) outstanding-nodes))))
             ;;
             (find-random-node ()
               (when (null outstanding-nodes)
                 (error "outstanding-nodes is empty when finding a random node"))
               (let* ((element (nth (mod (random:next-uint32 random) (length outstanding-nodes)) outstanding-nodes))
                      (node (receptacle:tree-find-node tree (car element))))
                 (unless node
                   (error "Element in outstanding-nodes is not in tree: ~s" element))
                 node)))
      ;;
      (loop
        for i from 0 below 100000
        ;; Simulated forward-flow of time with millisecond precision
        for current-time = 100000 then (+ current-time 1 (/ (1+ (mod (random:next-uint32 random) 500)) 1000))
        ;; Insertion of timer either 30 or 3 seconds from now
        for trigger-time = (if (zerop (mod (random:next-uint32 random) 2))
                               (+ current-time 30 (mod current-time 2))
                               (+ current-time 3 (mod current-time 2) 1))
        ;;
        ;; At random times, remove a random node
        when (and (not (receptacle:empty-p tree))
                  (zerop (mod (random:next-uint32 random) 10)))
          do (remove-node (find-random-node))
             ;;
             ;; Check for expired nodes
        do (loop
             for n = (and (not (receptacle:empty-p tree))
                          (receptacle:tree-first-node tree))
             while (and n (> current-time (car (receptacle:node-element n))))
             do (remove-node n))
           ;;
           ;; Insert the new node
        unless (receptacle:tree-find-node tree trigger-time)
          do (let ((element (list trigger-time i current-time)))
               (receptacle:tree-insert tree element)
               (push element outstanding-nodes))
             ;;
             ;; Verify the final state of the list
        finally (progn
                  (check-verifying-rbtree tree)
                  (fiveam:is (= (length outstanding-nodes)
                                (receptacle:content-length tree))))))))

(fiveam:test rbtree-large-test
  (let ((tree (make-instance 'verifying-rbtree :key #'car :test #'< :test-equal #'=))
        (random (make-instance 'acm-random:acm-random))
        (total 0))
    (loop
      repeat 1000
      for i from 0
      for v = (random:next-uint32 random)
      unless (receptacle:tree-find-node tree v)
        do (receptacle:tree-insert tree (list v i))
        and do (incf total))
    (fiveam:is (= total (receptacle:content-length tree)))
    (print total)
    (loop
      repeat total
      do (receptacle:tree-delete-node tree (receptacle:tree-first-node tree)))
    (fiveam:is (zerop (receptacle:content-length tree)))))
