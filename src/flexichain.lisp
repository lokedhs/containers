(in-package :receptacle.flexichain)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defmethod receptacle:content-length ((container flexichain:standard-flexichain))
  (flexichain:nb-elements container))

(defmethod receptacle:delete-all ((container flexichain:standard-flexichain))
  (flexichain:delete-elements* container 0 (flexichain:nb-elements container)))

(defmethod receptacle:empty-p ((container flexichain:standard-flexichain))
  (flexichain:flexi-empty-p container))

(defmethod receptacle:make-container-iterator ((container flexichain:standard-flexichain))
  (error "Iterator is not implemented"))

(defmethod receptacle:container-nth ((container flexichain:standard-flexichain) index)
  (flexichain:element* container index))

(defmethod (setf receptacle:container-nth) (element (container flexichain:standard-flexichain) index)
  (setf (flexichain:element* container index) element))
