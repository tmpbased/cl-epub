(in-package :test)

(defmacro with-symbols-from ((package-name &rest symbols) &body body)
  (let ((f-symbols (loop :for s :in symbols :collect 
					    (intern (symbol-name s) package-name))))
    `(progn
       ,@(loop :for symbol :in symbols :for f-symbol :in f-symbols 
	       :with body = body
	       :do (setf body (subst f-symbol symbol body :test #'eq))
	       :finally (return body)))))

(defun value-presence-eq (actual expected)
  (multiple-value-bind (expected-value expected-present-p) expected
    (multiple-value-bind (actual-value actual-present-p) actual
      (and (eq expected-value actual-value)
	   (eq expected-present-p actual-present-p)))))

(defmacro missing-value (form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
		   :expression '(missing-value ,form)
		   :value-form ',form
		   :body (lambda () ,form)
		   :expected (values nil nil)
		   :comparison 'value-presence-eq
		   ,@(when description
		       `(:description (format NIL ,description ,@format-args))))))

(defmacro has-default-value (default form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
		   :expression '(has-default-value ,form)
		   :value-form ',form
		   :body (lambda () ,form)
		   :expected (values ,default nil)
		   :comparison 'value-presence-eq
		   ,@(when description
		       `(:description (format NIL ,description ,@format-args))))))

(defmacro has-value (expected form &optional description &rest format-args)
  `(eval-in-context
    *context*
    (make-instance 'comparison-result
		   :expression '(has-value ,expected ,form)
		   :value-form ',form
		   :body (lambda () ,form)
		   :expected (values ,expected t)
		   :comparison 'value-presence-eq
		   ,@(when description
		       `(:description (format NIL ,description ,@format-args))))))
