(in-package :test)

(defmacro with-symbols-from ((package-name &rest symbols) &body body)
  (let ((f-symbols (loop :for s :in symbols :collect 
					    (intern (symbol-name s) package-name))))
    `(progn
       ,@(loop :for symbol :in symbols :for f-symbol :in f-symbols 
	       :with body = body
	       :do (setf body (subst f-symbol symbol body :test #'eq))
	       :finally (return body)))))
