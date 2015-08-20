(in-package :dhs-sequences)

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
  (:documentation "Returns an interator object that can be used to
traverse this container."))

;;;
;;;  Iterator functions
;;;

(defgeneric iterator/current-element (iterator)
  (:documentation "Returns the current element"))

(defgeneric iterator/get-next-element (iterator)
  (:documentation "Prepares the iterator to return the next element. Returns NIL if the endof the sequence has been reached."))
