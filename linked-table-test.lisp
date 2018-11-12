(in-package :test)

(define-test linked-table-suite)

(define-test t-empty-table
  :parent linked-table-suite
  (let ((table (make-linked-table (make-hash-table))))
    (true (table-p table))
    (is = 0 (table-count table))
    (missing-value (table-get :missing-key table))
    (has-default-value :default-value (table-get :missing-key table :default-value))))

(define-test t-table-add
  :parent linked-table-suite
  (let ((table (make-linked-table (make-hash-table))))
    (setf (table-get :a table) :b)
    (is = 1 (table-count table))
    (missing-value (table-get :b table))
    (has-value :b (table-get :a table))))

(define-test t-table-remove
  :parent linked-table-suite
  (let ((table (make-linked-table (make-hash-table))))
    (setf (table-get :a table) :b)
    (table-remove :b table)
    (is = 1 (table-count table))
    (has-value :b (table-get :a table))
    (table-remove :a table)
    (is = 0 (table-count table))
    (missing-value (table-get :a table))))

(define-test t-table-clear
  :parent linked-table-suite
  (let ((table (make-linked-table (make-hash-table))))
    (setf (table-get :a table) :b
	  (table-get :b table) :c)
    (is = 2 (table-count table))
    (has-value :b (table-get :a table))
    (has-value :c (table-get :b table))
    (table-clear table)
    (is = 0 (table-count table))
    (missing-value (table-get :a table))
    (missing-value (table-get :b table))))

(define-test t-table-map
  :parent linked-table-suite
  (let ((table (make-linked-table (make-hash-table)))
	(entries '((:a :b) (:b :c)))
	(iterations 0))
    (setf (table-get :a table) :b
	  (table-get :b table) :c)
    (table-map #'(lambda (key value)
		   (destructuring-bind (expected-key expected-value) (car entries)
		     (is eq expected-key key)
		     (is eq expected-value value)
		     (setf entries (cdr entries))
		     (incf iterations)))
	       table)
    (is = 2 iterations)))
