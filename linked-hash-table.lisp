(in-package :linked-hash-table)

(defgeneric gethash (key hash-table &optional default))

(defgeneric (setf gethash) (new-value key hash-table &optional default))

(defgeneric remhash (key hash-table))

(defmethod gethash (key (hash-table hash-table) &optional default)
  (cl:gethash key hash-table default))

(defmethod (setf gethash) (new-value key (hash-table hash-table) &optional default)
  (setf (cl:gethash key hash-table) new-value))

(defmethod remhash (key (hash-table hash-table))
  (cl:remhash key hash-table))

(defstruct (linked-hash-table (:conc-name lht-))
  (hash-table (make-hash-table) :type hash-table :read-only t)
  (iter-list (make-queue) :type queue::queue :read-only t))

(defmethod gethash (key (hash-table linked-hash-table) &optional default)
  (let ((ht (lht-hash-table hash-table)))
    (cl:gethash key ht default)))

(defmethod (setf gethash) (new-value key (hash-table linked-hash-table) &optional default)
  (let ((ht (lht-hash-table hash-table))
	(q (lht-iter-list hash-table)))
    (multiple-value-bind (old-value present-p) (cl:gethash key ht)
      (unless present-p
	(queue-push key q)))
    (setf (cl:gethash key ht) new-value)))
