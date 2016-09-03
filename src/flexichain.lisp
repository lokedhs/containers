(in-package :receptacle.flexichain)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defmethod receptacle:content-length ((container flexichain:flexichain))
  (flexichain:nb-elements container))

(defmethod receptacle:delete-all ((container flexichain:flexichain))
  (flexichain:delete-elements* container 0 (flexichain:nb-elements container)))

(defmethod receptacle:empty-p ((container flexichain:flexichain))
  (flexichain:flexi-empty-p container))

(defmethod receptacle:make-container-iterator ((container flexichain:flexichain))
  (make-instance 'receptacle::random-access-iterator :container container))

(defmethod receptacle:container-nth ((container flexichain:flexichain) index)
  (flexichain:element* container index))

(defmethod (setf receptacle:container-nth) (element (container flexichain:flexichain) index)
  (setf (flexichain:element* container index) element))

(defmethod receptacle:insert-at-position ((container flexichain:flexichain) index element)
  (flexichain:insert* container index element))

(defmethod receptacle:remove-at-position ((container flexichain:flexichain) index)
  (flexichain:delete* container index))
