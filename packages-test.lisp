(defpackage :test
  (:use #:common-lisp
	#:html-gen
	#:epub
	#:linked-table
	#:alexandria
	#:parachute)
  (:shadowing-import-from #:parachute :of-type)
  (:export #:check
	   #:deftest))
