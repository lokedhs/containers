(in-package :dhs-sequences.red-black-tree)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(defclass node ()
  ((value  :type t
           :initarg :value
           :reader node/value)
   (red    :type (member t nil)
           :initarg :red
           :accessor node/red)
   (left   :type node
           :accessor node/left
           :initarg :left)
   (right  :type node
           :accessor node/right
           :initarg :right)
   (parent :type node
           :accessor node/parent
           :initarg :parent)))

(defmethod print-object ((obj node) stream)
  (declare (type stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "VALUE ~s" (if (slot-boundp obj 'value)
                                  (slot-value obj 'value)
                                  :not-bound))))

(defclass red-black-tree (container)
  ((root          :type node
                  :accessor red-black-tree/root)
   (empty-node    :type node
                  :reader red-black-tree/empty-node)
   (test-equal-fn :type function
                  :initform #'string=
                  :initarg :test-equal
                  :reader red-black-tree/test-equal-fn)
   (test-fn       :type function
                  :initform #'string<
                  :initarg :test
                  :reader red-black-tree/test-fn)
   (key-fn        :type function
                  :initform #'identity
                  :initarg :key
                  :reader red-black-tree/key-fn)))

(defun rb-clear-tree (obj)
  (let ((e (make-instance 'node :red nil)))
    (setf (slot-value obj 'empty-node) e)
    (setf (node/parent e) e)
    (setf (node/left e) e)
    (setf (node/right e) e)
    (let ((root (make-instance 'node :red nil)))
      (setf (red-black-tree/root obj) root)
      (setf (node/parent root) e)
      (setf (node/left root) e)
      (setf (node/right root) e))))

(defmethod initialize-instance :after ((obj red-black-tree) &key)
  (rb-clear-tree obj))

(defun rb-left-rotate (tree x)
  (check-type tree red-black-tree)
  (check-type x node)
  (let ((e (red-black-tree/empty-node tree))
        (y (node/right x)))
    (setf (node/right x) (node/left y))
    (unless (eq (node/left y) e)
      (setf (node/parent (node/left y)) x))
    (setf (node/parent y) (node/parent x))
    (if (eq x (node/left (node/parent x)))
        (setf (node/left (node/parent x)) y)
        (setf (node/right (node/parent x)) y))
    (setf (node/left y) x)
    (setf (node/parent x) y)))

(defun rb-right-rotate (tree y)
  (check-type tree red-black-tree)
  (check-type y node)
  (let ((e (red-black-tree/empty-node tree))
        (x (node/left y)))
    (setf (node/left y) (node/right x))
    (unless (eq (node/right x) e)
      (setf (node/parent (node/right x)) y))
    (setf (node/parent x) (node/parent y))
    (if (eq y (node/left (node/parent y)))
        (setf (node/left (node/parent y)) x)
        (setf (node/right (node/parent y)) x))
    (setf (node/right x) y)
    (setf (node/parent y) x)))

(defun rb-insert-help (tree z)
  (check-type tree red-black-tree)
  (check-type z node)
  (let ((e (red-black-tree/empty-node tree)))
    (setf (node/left z) e)
    (setf (node/right z) e)
    (let ((test-fn (red-black-tree/test-fn tree))
          (key-fn (red-black-tree/key-fn tree))
          (y (red-black-tree/root tree)))
      (declare (type function test-fn key-fn))
      (loop
         with x = (node/left y)
         until (eq x e)
         do (progn
              (setf y x)
              (if (funcall test-fn (funcall key-fn (node/value z)) (funcall key-fn (node/value x)))
                  (setf x (node/left x))
                  (setf x (node/right x)))))
      (setf (node/parent z) y)
      (if (or (eq y (red-black-tree/root tree))
              (funcall test-fn (funcall key-fn (node/value z)) (funcall key-fn (node/value y))))
          (setf (node/left y) z)
          (setf (node/right y) z)))))

(defun rb-insert-node (tree value)
  (check-type tree red-black-tree)
  (let ((x (make-instance 'node :value value)))
    (rb-insert-help tree x)
    (let ((new-node x))
      (setf (node/red x) t)
      (loop
         while (node/red (node/parent x))
         if (eq (node/parent x) (node/left (node/parent (node/parent x))))
         do (progn
              (let ((y (node/right (node/parent (node/parent x)))))
                (if (node/red y)
                    (progn
                      (setf (node/red (node/parent x)) nil)
                      (setf (node/red y) nil)
                      (setf (node/red (node/parent (node/parent x))) t)
                      (setf x (node/parent (node/parent x))))
                    (progn
                      (when (eq x (node/right (node/parent x)))
                        (setf x (node/parent x))
                        (rb-left-rotate tree x))
                      (setf (node/red (node/parent x)) nil)
                      (setf (node/red (node/parent (node/parent x))) t)
                      (rb-right-rotate tree (node/parent (node/parent x)))))))
         else
         do (let ((y (node/left (node/parent (node/parent x)))))
              (if (node/red y)
                  (progn
                    (setf (node/red (node/parent x)) nil)
                    (setf (node/red y) nil)
                    (setf (node/red (node/parent (node/parent x))) t)
                    (setf x (node/parent (node/parent x))))
                  (progn
                    (when (eq x (node/left (node/parent x)))
                      (setf x (node/parent x))
                      (rb-right-rotate tree x))
                    (setf (node/red (node/parent x)) nil)
                    (setf (node/red (node/parent (node/parent x))) t)
                    (rb-left-rotate tree (node/parent (node/parent x)))))))
      (setf (node/red (node/left (red-black-tree/root tree))) nil)
      new-node)))

(defun rb-successor (tree x)
  (check-type tree red-black-tree)
  (check-type x node)
  (let ((e (red-black-tree/empty-node tree))
        (root (red-black-tree/root tree))
        (y (node/right x)))
    (if (not (eq y e))
        (loop
           until (eq (node/left y) e)
           do (setf y (node/left y))
           finally (return y))
        (progn
          (setf y (node/parent x))
          (loop
             while (eq x (node/right y))
             do (progn
                  (setf x y)
                  (setf y (node/parent y)))
             finally (return (if (eq y root) e y)))))))

(defun rb-exact-query (tree key)
  (let ((e (red-black-tree/empty-node tree))
        (x (node/left (red-black-tree/root tree)))
        (test-fn (red-black-tree/test-fn tree))
        (test-equal-fn (red-black-tree/test-equal-fn tree))
        (key-fn (red-black-tree/key-fn tree)))
    (declare (type function test-fn test-equal-fn key-fn))
    (if (eq x e)
        nil
        (loop
           for key-x = (funcall key-fn (node/value x))
           until (funcall test-equal-fn key-x key)
           if (funcall test-fn key key-x)
           do (setf x (node/left x))
           else do (setf x (node/right x))
           if (eq x e)
           do (return nil)
           finally (return x)))))

(defun rb-first-node (tree)
  (let ((e (red-black-tree/empty-node tree))
        (x (red-black-tree/root tree)))
    (if (eq (node/left x) e)
        e
        (loop
           do (setf x (node/left x))
           until (eq (node/left x) e)
           finally (return x)))))

(defun rb-delete-fixup (tree x)
  (check-type tree red-black-tree)
  (check-type x node)
  (loop
     with root = (red-black-tree/root tree)
     while (and (not (node/red x))
                (not (eq root x)))
     if (eq x (node/left (node/parent x)))
     do (let ((w (node/right (node/parent x))))
          (when (node/red w)
            (setf (node/red w) nil)
            (setf (node/red (node/parent x)) t)
            (rb-left-rotate tree (node/parent x))
            (setf w (node/right (node/parent x))))
          (if (and (not (node/red (node/right w)))
                   (not (node/red (node/left w))))
              (progn
                (setf (node/red w) t)
                (setf x (node/parent x)))
              (progn
                (when (not (node/red (node/right w)))
                  (setf (node/red (node/left w)) nil)
                  (setf (node/red w) t)
                  (rb-right-rotate tree w)
                  (setf w (node/right (node/parent x))))
                (setf (node/red w) (node/red (node/parent x)))
                (setf (node/red (node/parent x)) nil)
                (setf (node/red (node/right w)) nil)
                (rb-left-rotate tree (node/parent x))
                (setf x root))))
     else
     do (let ((w (node/left (node/parent x))))
          (when (node/red w)
            (setf (node/red w) nil)
            (setf (node/red (node/parent x)) t)
            (rb-right-rotate tree (node/parent x))
            (setf w (node/left (node/parent x))))
          (if (and (not (node/red (node/right w)))
                   (not (node/red (node/left w))))
              (progn
                (setf (node/red w) t)
                (setf x (node/parent x)))
              (progn
                (when (not (node/red (node/left w)))
                  (setf (node/red (node/right w)) nil)
                  (setf (node/red w) t)
                  (rb-left-rotate tree w)
                  (setf w (node/left (node/parent x))))
                (setf (node/red w) (node/red (node/parent x)))
                (setf (node/red (node/parent x)) nil)
                (setf (node/red (node/left w)) nil)
                (rb-right-rotate tree (node/parent x))
                (setf x root)))))
  (setf (node/red x) nil))

(defun rb-delete (tree z)
  (check-type tree red-black-tree)
  (check-type z node)
  (let* ((e (red-black-tree/empty-node tree))
         (root (red-black-tree/root tree))
         (y (if (or (eq (node/left z) e)
                    (eq (node/right z) e))
                z
                (rb-successor tree z)))
         (x (if (eq (node/left y) e)
                (node/right y)
                (node/left y))))
    (setf (node/parent x) (node/parent y))
    (if (eq root (node/parent x))
        (setf (node/left root) x)
        (if (eq y (node/left (node/parent y)))
            (setf (node/left (node/parent y)) x)
            (setf (node/right (node/parent y)) x)))
    (if (not (eq z y))
        (progn
          (when (not (node/red y))
            (rb-delete-fixup tree x))
          (setf (node/left y) (node/left z))
          (setf (node/right y) (node/right z))
          (setf (node/parent y) (node/parent z))
          (setf (node/red y) (node/red z))
          (setf (node/parent (node/right z)) y)
          (setf (node/parent (node/left z)) y)
          (if (eq z (node/left (node/parent z)))
              (setf (node/left (node/parent z)) y)
              (setf (node/right (node/parent z)) y)))
        (when (not (node/red y))
          (rb-delete-fixup tree x)))))

;;;
;;;  API implementation
;;;

(defmethod dhs-sequences:tree-insert ((tree red-black-tree) element)
  (rb-insert-node tree element))

(defmethod dhs-sequences:tree-find-node ((tree red-black-tree) key)
  (rb-exact-query tree key))

(defmethod dhs-sequences:tree-find-element ((tree red-black-tree) key)
  (let ((node (rb-exact-query tree key)))
    (if node
        (node/value node)
        nil)))

(defmethod dhs-sequences:tree-delete-node ((tree red-black-tree) node)
  (check-type node node)
  (rb-delete tree node))

(defmethod dhs-sequences:tree-delete-element ((tree red-black-tree) element)
  (let ((node (rb-exact-query tree element)))
    (if node
        (rb-delete tree node)
        nil)))

(defmethod dhs-sequences:tree-first-node ((tree red-black-tree))
  (let ((node (rb-first-node tree)))
    (if (eq node (red-black-tree/empty-node tree))
        nil
        node)))

(defmethod dhs-sequences:tree-first-element ((tree red-black-tree))
  (let ((node (rb-first-node tree)))
    (if (eq node (red-black-tree/empty-node tree))
        nil
        (node/value node))))

(defmethod dhs-sequences:tree-next ((tree red-black-tree) node)
  (check-type node node)
  (let ((node (rb-successor tree node)))
    (if (eq node (red-black-tree/empty-node tree))
        nil
        node)))

(defmethod dhs-sequences:node-element ((node node))
  (node/value node))

(defmethod dhs-sequences:content-length ((tree red-black-tree))
  (let ((e (red-black-tree/empty-node tree)))
    (loop
       for node = (rb-first-node tree) then (rb-successor tree node)
       until (eq node e)
       summing 1)))

(defmethod dhs-sequences:delete-all ((tree red-black-tree))
  (rb-clear-tree tree))
