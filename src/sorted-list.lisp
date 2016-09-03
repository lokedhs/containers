(in-package :receptacle)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass sorted-list (test-fn-mixin)
  ((content :initarg :content
            :initform (error "~s required when creating ~s" :content 'sorted-list)
            :reader sorted-list/content)))

(defmethod print-object ((obj sorted-list) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (if (slot-boundp obj 'content)
                            (slot-value obj 'content)
                            :not-bound))))

(defun sorted-list-bsearch (container key)
  "Perform a bsearch across the content of CONTAINER, searching for KEY.
Returns three values: If found: The found object, its index, T. If not found:
NIL, the index where the object should be if inserted, NIL."
  (let ((content (sorted-list/content container))
        (test-fn (test-fn-mixin/test-fn container))
        (test-equal-fn (test-fn-mixin/test-equal-fn container))
        (key-fn (test-fn-mixin/key-fn container)))
    (labels ((search-span (start end)
               (let ((w (- end start)))
                 (cond ((zerop w)
                        (values nil start nil))
                       (t
                        (let* ((mid (truncate (+ start (/ w 2))))
                               (element (container-nth content mid))
                               (key-from-element (funcall key-fn element)))
                          (cond ((funcall test-fn key-from-element key)
                                 ;; the midpoint element is less than the key
                                 (search-span (1+ mid) end))
                                ((funcall test-equal-fn key-from-element key)
                                 ;; the midpoint is equal to the key
                                 (values element mid t))
                                (t
                                 ;; the midpoint element is greater than the key
                                 (search-span start mid)))))))))
      (search-span 0 (content-length content)))))

(macrolet ((define-delegate-function (name args)
             (alexandria:with-gensyms (v)
               `(defmethod ,name (,v ,@args)
                  (,name (sorted-list/content ,v) ,@args)))))
  (define-delegate-function content-length ())
  (define-delegate-function empty-p ())
  (define-delegate-function container-nth (index))
  (define-delegate-function remove-at-position (index))
  (define-delegate-function delete-all ()))

(defmethod tree-insert ((container sorted-list) element)
  (let ((content (sorted-list/content container)))
    (multiple-value-bind (e index found-p)
        (sorted-list-bsearch container (funcall (test-fn-mixin/key-fn container) element))
      (declare (ignore e))
      (if found-p
          (setf (container-nth content index) element)
          (insert-at-position content index element)))))

(defmethod tree-delete-element ((container sorted-list) key)
  (multiple-value-bind (e index found-p)
      (sorted-list-bsearch container key)
    (declare (ignore e))
    (when found-p
      (remove-at-position (sorted-list/content container) index))))

(defmethod make-container-iterator ((container sorted-list))
  (make-container-iterator (sorted-list/content container)))
