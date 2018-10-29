;;;; cl-epub.asd

(asdf:defsystem #:cl-epub
  :description "Describe cl-epub here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (alexandria)
  :components
  ((:file "packages")
   (:file "html-gen")
   (:file "epub")
   (:file "classes")
   (:file "methods"))
  :in-order-to ((test-op (test-op "cl-epub-test"))))

