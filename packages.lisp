(defpackage :html-gen
  (:use #:common-lisp
	#:alexandria)
  (:export #:html
	   #:xml
	   #:xml-declaration
	   #:generate-html
	   #:generate-xml))

(defpackage :epub
  (:use #:common-lisp
	#:html-gen
	#:alexandria)
  (:export #:write-mimetype
	   #:write-container-xml
	   #:write-content
	   #:write-package-document
	   #:create-metadata))
