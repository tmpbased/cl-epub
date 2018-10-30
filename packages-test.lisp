(defpackage :test
  (:use #:common-lisp
	#:html-gen
	#:epub
	#:alexandria
	#:parachute)
  (:shadowing-import-from #:parachute :of-type)
  (:export #:check
	   #:deftest))
