(defpackage :linked-table
  (:use #:common-lisp
	#:alexandria)
  (:export #:make-linked-table
	   #:table-get
	   #:table-remove
	   #:table-clear
	   #:table-map
	   #:table-count
	   #:table-p))

(defpackage :html-gen
  (:use #:common-lisp
	#:alexandria
	#:linked-table)
  (:export #:html
	   #:xml
	   #:xml-declaration
	   #:generate-html
	   #:generate-xml))

(defpackage :epub
  (:use #:common-lisp
	#:html-gen
	#:linked-table
	#:alexandria)
  (:export #:write-mimetype
	   #:write-container-xml
	   #:write-content
	   #:write-package-document
	   #:create-metadata))
