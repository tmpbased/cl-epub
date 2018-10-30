;;;; cl-epub-test.asd

(asdf:defsystem #:cl-epub-test
  :description "Describe cl-epub-test here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (alexandria parachute cl-epub)
  :components
  ((:file "packages-test")
   (:file "testing")
   (:file "html-gen-test")
   (:file "epub-test")
   (:file "classes-test")
   (:file "methods-test"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :test)))

