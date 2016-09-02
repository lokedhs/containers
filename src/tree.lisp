(in-package :receptacle)

(defclass tree (container)
  ()
  (:documentation "Generic tree class that provides implementations of
generic functions that are implemented through the standard tree
API."))

(defclass tree-iterator ()
  ((tree    :type t
            :initarg :tree
            :reader tree-iterator/tree)
   (current :type t
            :initarg :current
            :accessor tree-iterator/current)))


(defun make-tree-iterator (tree)
  (make-instance 'tree-iterator :tree tree :current (tree-first-node tree)))

(defmethod iterator/get-next-element ((iterator tree-iterator))
  (let ((current (tree-iterator/current iterator)))
    (cond (current
           (setf (tree-iterator/current iterator) (tree-next (tree-iterator/tree iterator) current))
           (node-element current))
          (t
           (values nil t)))))

(defmethod make-container-iterator ((container tree))
  (make-tree-iterator container))

(defun find-tree-node-at-position (tree index)
  (loop
    for i from 0 to index
    for current = (tree-first-node tree) then (tree-next tree current)
    when (null current)
      do (error 'index-error :index index)
    finally (return i)))

(defmethod container-nth ((container tree) index)
  (node-element (find-tree-node-at-position container index)))
