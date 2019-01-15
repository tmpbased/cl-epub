(in-package :linked-table)

;; DEQUE

(defstruct (deque (:constructor %make-deque ())) first last)

(defstruct (deque-node (:constructor %make-deque-node (value deque &key (previous nil) (next nil))))
  (value value :read-only t)
  (deque deque :type deque :read-only t)
  (previous previous)
  (next next))

(defmethod print-object ((node deque-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~a" (deque-node-value node))))

(defun make-deque ()
  (%make-deque))

(defun deque-empty? (deque)
  (null (deque-first deque)))

(defun deque-pop-first (deque)
  (unless (deque-empty? deque)
    (let* ((first-node (deque-first deque))
	   (next-node (deque-node-next first-node)))
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
      (let ((previous-node (deque-node-previous deque-node))
	    (next-node (deque-node-next deque-node)))
	(if previous-node
	    (setf (deque-node-next previous-node) next-node)
	    (setf (deque-first deque) next-node))
	(if next-node
	    (setf (deque-node-previous next-node) previous-node)
	    (setf (deque-last deque) previous-node)))
      (deque-node-value deque-node))))

(defun deque-clear (deque)
  (setf
   (deque-first deque) nil
   (deque-last deque) nil))

;; TABLE

(defgeneric table-get (key table &optional default))

(defgeneric (setf table-get) (new-value key table &optional default))

(defgeneric table-remove (key table))

(defgeneric table-clear (table))

(defgeneric table-map (fn table))

(defgeneric table-p (table))

(defgeneric table-count (table))

(defmethod table-p (table)
  nil)

;; TABLE for HASH-TABLE

(defmethod table-get (key (table hash-table) &optional default)
  (gethash key table default))

(defmethod (setf table-get) (new-value key (table hash-table) &optional default)
  (declare (ignore default))
  (setf (gethash key table) new-value))

(defmethod table-remove (key (table hash-table))
  (remhash key table))

(defmethod table-clear ((table hash-table))
  (clrhash table))

(defmethod table-map (fn (table hash-table))
  (maphash fn table))

(defmethod table-p ((table hash-table))
  t)

(defmethod table-count ((table hash-table))
  (hash-table-count table))

;; LINKED-TABLE

(defstruct (linked-table (:conc-name lt-) (:constructor %make-linked-table (table)))
  (table table :read-only t)
  (iter-seq (make-deque) :type deque :read-only t))

(defun make-linked-table (table)
  (%make-linked-table table))

(defstruct (linked-value (:conc-name lv-) (:constructor %make-linked-value (value deque-node)))
  (value value)
  (iter-node deque-node :type deque-node :read-only t))

;; TABLE for LINKED-TABLE

(defmethod table-get (key (table linked-table) &optional default)
  (let ((the-table (lt-table table)))
    (multiple-value-bind (value present-p) (gethash key the-table)
      (values
       (if present-p
	   (lv-value value)
	   default)
       present-p))))

(defmethod (setf table-get) (new-value key (table linked-table) &optional default)
  (declare (ignore default))
  (let* ((the-table (lt-table table))
	 (q (lt-iter-seq table))
	 (deque-node
	   (multiple-value-bind (old-value present-p) (table-get key the-table)
	     (declare (ignore old-value))
	     (unless present-p
	       (deque-push-last key q))))
	 (linked-value (%make-linked-value new-value deque-node)))
    (setf (table-get key the-table) linked-value)))

(defmethod table-remove (key (table linked-table))
  (let ((the-table (lt-table table)))
    (multiple-value-bind (old-value present-p) (table-get key the-table)
      (when present-p
	(deque-remove-node (lv-iter-node old-value))))
    (table-remove key the-table)))

(defmethod table-clear ((table linked-table))
  (let ((the-table (lt-table table))
	(q (lt-iter-seq table)))
    (table-clear the-table)
    (deque-clear q)
    table))

(defmethod table-map (fn (table linked-table))
  (for:for ((e over table))
    (destructuring-bind (k v) e
      (funcall fn k v))))

(defmethod table-p ((table linked-table))
  t)

(defmethod table-count ((table linked-table))
  (let ((the-table (lt-table table)))
    (table-count the-table)))

;; FOR:ITERATOR for LINKED-TABLE

(defclass table-iterator (for:iterator)
  ((table :initarg :table :reader table)))

(defmethod for:has-more ((iterator table-iterator))
  (not (null (for:object iterator))))

(defmethod for:next ((iterator table-iterator))
  (with-accessors ((node for:object)) iterator
    (let* ((key (deque-node-value node))
	   (table (table iterator))
	   (value (table-get key table)))
      (setf node (deque-node-next node))
      (list key value))))

(defmethod (setf for:current) (value (iterator table-iterator))
  (let* ((node (for:object iterator))
	 (table (table iterator))
	 (key (deque-node-value node)))
    (setf (table-get key table) value)))

(defmethod for:make-iterator ((table linked-table) &key)
  (make-instance 'table-iterator :object (deque-first (lt-iter-seq table)) :table table))
