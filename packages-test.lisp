(defpackage :test
  (:use #:common-lisp
	#:alexandria)
  (:export #:check
	   #:deftest))

(defpackage :html-gen
  (:use #:common-lisp
	#:alexandria
	#:test)
  (:export #:html
	   #:xml
	   #:xml-declaration
	   #:generate-html
	   #:generate-xml))

(defpackage :epub
  (:use #:common-lisp
	#:alexandria
	#:html-gen
	#:test)
  (:export #:write-mimetype
	   #:write-container-xml
	   #:write-content
	   #:write-package-document
	   #:create-metadata))
