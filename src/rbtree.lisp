(in-package :dhs-sequences.red-black-tree)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass node ()
  ((value  :type t
           :initarg :value
           :reader node/value)
   (red    :type (member t nil)
           :initform nil
           :initarg :red
           :accessor node/red)
   (left   :type (or null node)
           :initform nil
           :accessor node/left
           :initarg :left)
   (right  :type (or null node)
           :initform nil
           :accessor node/right
           :initarg :right)
   (parent :type (or null node)
           :initform nil
           :accessor node/parent
           :initarg :parent)))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "VALUE ~s" (if (slot-boundp obj 'value)
                                  (slot-value obj 'value)
                                  :not-bound))))

(defclass red-black-tree (container)
  ((root          :type node
                  :initform (make-instance 'node :red nil)
                  :accessor red-black-tree/root)
   (empty-node    :type node
                  :initform (make-instance 'node :red nil)
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

(defmethod initialize-instance :after ((obj red-black-tree) &key)
  (let ((e (red-black-tree/empty-node obj)))
    (setf (node/parent e) e)
    (setf (node/left e) e)
    (setf (node/right e) e)
    (let ((root (red-black-tree/root obj)))
      (setf (node/parent root) e)
      (setf (node/left root) e)
      (setf (node/right root) e))))

(defun left-rotate (tree x)
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

(defun right-rotate (tree y)
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

(defun tree-insert-help (tree z)
  (check-type tree red-black-tree)
  (check-type z node)
  (let ((e (red-black-tree/empty-node tree)))
    (setf (node/left z) e)
    (setf (node/right z) e)
    (let ((test-fn (red-black-tree/test-fn tree))
          (key-fn (red-black-tree/key-fn tree))
          (y (red-black-tree/root tree)))
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

(defun tree-insert (tree value)
  (check-type tree red-black-tree)
  (let ((x (make-instance 'node :value value)))
    (tree-insert-help tree x)
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
                        (left-rotate tree x))
                      (setf (node/red (node/parent x)) nil)
                      (setf (node/red (node/parent (node/parent x))) t)
                      (right-rotate tree (node/parent (node/parent x)))))))
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
                      (right-rotate tree x))
                    (setf (node/red (node/parent x)) nil)
                    (setf (node/red (node/parent (node/parent x))) t)
                    (left-rotate tree (node/parent (node/parent x)))))))
      (setf (node/red (node/left (red-black-tree/root tree))) nil)
      new-node)))

(defun tree-successor (tree x)
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

(defun exact-query (tree key)
  (let ((e (red-black-tree/empty-node tree))
        (x (node/left (red-black-tree/root tree)))
        (test-fn (red-black-tree/test-fn tree))
        (test-equal-fn (red-black-tree/test-equal-fn tree))
        (key-fn (red-black-tree/key-fn tree)))
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

(defun tree-first-node (tree)
  (let ((e (red-black-tree/empty-node tree))
        (x (red-black-tree/root tree)))
    (if (eq (node/left x) e)
        e
        (loop
           do (setf x (node/left x))
           until (eq (node/left x) e)
           finally (return x)))))

(defun tree-delete-fixup (tree x)
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
            (left-rotate tree (node/parent x))
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
                  (right-rotate tree w)
                  (setf w (node/right (node/parent x))))
                (setf (node/red w) (node/red (node/parent x)))
                (setf (node/red (node/parent x)) nil)
                (setf (node/red (node/right w)) nil)
                (left-rotate tree (node/parent x))
                (setf x root))))
     else
     do (let ((w (node/left (node/parent x))))
          (when (node/red w)
            (setf (node/red w) nil)
            (setf (node/red (node/parent x)) t)
            (right-rotate tree (node/parent x))
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
                  (setf w (node/left (node/parent x))))
                (setf (node/red w) (node/red (node/parent x)))
                (setf (node/red (node/parent x)) nil)
                (setf (node/red (node/left w)) nil)
                (right-rotate tree (node/parent x))
                (setf x root)))))
  (setf (node/red x) nil))

(defun tree-delete (tree z)
  (check-type tree red-black-tree)
  (check-type z node)
  (let* ((e (red-black-tree/empty-node tree))
         (root (red-black-tree/root tree))
         (y (if (or (eq (node/left z) e)
                    (eq (node/right z) e))
                z
                (tree-successor tree z)))
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
            (tree-delete-fixup tree x))
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
          (tree-delete-fixup tree x)))))
