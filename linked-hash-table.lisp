(in-package :linked-hash-table)

(defstruct deque first last)

(defstruct (deque-node (:constructor %make-deque-node (value deque &key (previous nil) (next nil))))
  (value value :read-only t)
  (deque deque :type deque :read-only t)
  (previous previous)
  (next next))

(defmethod print-object ((node deque-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~a" (deque-node-value node))))

(defun deque-empty? (deque)
  (null (deque-first deque)))

(defun deque-pop-first (deque)
  (unless (deque-empty? deque)
    (let* ((first-node (deque-first deque))
	   (next-node (deque-node-next first-node))
	   (last-node (deque-last deque)))
      (setf (deque-first deque) next-node)
      (unless next-node
	(setf (deque-last deque) nil))
      (deque-node-value first-node))))

(defun deque-push-last (value deque)
  (let* ((last-node (deque-last deque))
	 (new-node (%make-deque-node value deque :previous last-node)))
    (setf (deque-last deque) new-node)
    (if last-node
	(setf (deque-node-next last-node) new-node)
	(setf (deque-first deque) new-node))
    new-node))

(defun deque-remove-node (deque-node)
  (let ((deque (deque-node-deque deque-node)))
    (unless (deque-empty? deque)
      (let ((first-node (deque-first deque))
	    (last-node (deque-last deque))
	    (previous-node (deque-node-previous deque-node))
	    (next-node (deque-node-next deque-node)))
	(if previous-node
	    (setf (deque-node-next previous-node) next-node)
	    (setf (deque-first deque) next-node))
	(if next-node
	    (setf (deque-node-previous next-node) previous-node)
	    (setf (deque-last deque) previous-node)))
      (deque-node-value deque-node))))

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
