(in-package :receptacle)

(define-condition receptacle-error (error)
  ())

(define-condition index-error (receptacle-error)
  ((index :type integer
          :initarg :index
          :reader index-error/index))
  (:report (lambda (condition stream)
             (format stream "Illegal index: ~a" (index-error/index condition)))))

(defclass container ()
  ()
  (:documentation "Superclass for all containers"))

(defgeneric content-length (container)
  (:documentation "Returns the number of elements in CONTAINER"))

(defgeneric delete-all (container)
  (:documentation "Removes all entries from CONTAINER"))

(defgeneric empty-p (container)
  (:documentation "Returns non-NIL if CONTAINER is empty"))

(defmethod empty-p ((container container))
  "Default implementation based that uses CONTENT-LENGTH as backend"
  (zerop (content-length container)))

(defgeneric make-container-iterator (container)
  (:documentation "Returns an iterator object that can be used to
traverse this container."))

;;;
;;;  Sequence functions
;;;

(defgeneric container-nth (container index)
  (:documentation "Returns the element at index INDEX in the container."))

(defgeneric insert-at-position (container index element)
  (:documentation "Inserts ELEMENT at position INDEX in CONTAINER.
The index of every element after the insertion position gets incremented by 1."))

(defgeneric remove-at-position (container index)
  (:documentation "Removes the elementa at position INDEX from CONTAINER.
The index of every element after the remove position is decremented by 1."))

(defgeneric (setf container-nth) (element container index)
  (:documentation "Sets the value at index INDEX in CONTAINER to ELEMENT."))

;;;
;;;  Queue functions
;;;

(defgeneric queue-push (queue element)
  (:documentation "Push ELEMENT to the tail of QUEUE"))

(defgeneric queue-pop (queue &key if-empty)
  (:documentation "Pop the element from the head of QUEUE"))

;;;
;;;  Tree functions
;;;

(defgeneric tree-insert (tree element)
  (:documentation "Add ELEMENT to TREE"))

(defgeneric tree-find-node (tree key)
  (:documentation "Return the node for KEY in TREE, or NIL if the node does not exist."))

(defgeneric tree-find-element (tree key)
  (:documentation "Return the element for KEY in TREE, or NIL if the node does not exist."))

(defgeneric tree-delete-node (tree node)
  (:documentation "Remove NODE from TREE."))

(defgeneric tree-delete-element (tree element)
  (:documentation "Remove the node corresponding to ELEMENT from TREE.
Returns the value from the removed node or NIL if the element could
not be found in the tree."))

(defgeneric tree-first-node (tree)
  (:documentation "Returns the first node in TREE, or NIL if the tree is empty."))

(defgeneric tree-first-element (tree)
  (:documentation "Returns the value in the first node of TREE, or NIL if the tree is empty."))

(defgeneric tree-last-node (tree)
  (:documentation "Returns the last node in TREE, or NIL if the tree is empty."))

(defgeneric tree-last-element (tree)
  (:documentation "Returns the value in the last node of TREE, or NIL if the tree is empty."))

(defgeneric tree-next (tree node)
  (:documentation "Returns the subsequent node after NODE in TREE or NIL if this is the last node."))

(defgeneric tree-previous (tree node)
  (:documentation "Returns the prior node after NODE in TREE or NIL if this is the first node."))

(defgeneric node-element (node)
  (:documentation "Returns the element corresponding to NODE."))

;;;
;;;  Iterator functions
;;;

(defgeneric iterator/get-next-element (iterator)
  (:documentation "Prepares the iterator to return the next element.
If the iterator is not at the end of the list, returns the value and NIL.
If the end of the list was reached, returns NIL and T."))
