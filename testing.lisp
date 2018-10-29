;;; This testing framework was defined in Practical Common Lisp by Peter Seibel
(in-package :test)

(defparameter *test-name* nil)

(defun report-result (result form)
  (format *error-output* "~:[FAIL ~a: ~a~%~;~]" result *test-name* form)
  result)

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))
