(in-package :test)

(define-test html-gen-suite)

(define-test t-html-statement-p
  :parent html-gen-suite
  (with-symbols-from (:html-gen html-statement-p)
    ;; this should work
    (true (html-statement-p '(:p () "foo")))

    ;; and this
    (true (html-statement-p '(:a ((:href "foo")) "bar")))

    ;; check something that is not a list
    (false (html-statement-p '"foo"))

    ;; check with args not a list
    (false (html-statement-p '(:p "foo" "bar")))

    ;; check with not a keyword
    (false (html-statement-p '(format nil "foo")))))

(define-test t-create-attributes-string
  :parent html-gen-suite
  (with-symbols-from (:html-gen create-attributes-string)
    ;; test with the empty list
    (is string= "" (create-attributes-string ()))

    ;; test with one attribute
    (is string= " foo=\"bar\"" (create-attributes-string ((:foo "bar"))))

    ;; test with two attributes
    (is string= " foo=\"bar\" monkey=\"banana\""
	(create-attributes-string ((:foo "bar") (:monkey "banana"))))

    ;; test with bound variables
    (let ((foo "bar")
	  (monkey "banana"))
      (is string= " foo=\"bar\" monkey=\"banana\""
	  (create-attributes-string ((:foo foo) (:monkey monkey)))))))

(define-test t-void-element-p
  :parent html-gen-suite
  (with-symbols-from (:html-gen void-element-p)
    ;; check all the void elements are found
    (true (and (void-element-p :area)
	       (void-element-p :base)
	       (void-element-p :br)
	       (void-element-p :col)
	       (void-element-p :command)
	       (void-element-p :embed)
	       (void-element-p :hr)
	       (void-element-p :img)
	       (void-element-p :input)
	       (void-element-p :keygen)
	       (void-element-p :link)
	       (void-element-p :meta)
	       (void-element-p :param)
	       (void-element-p :source)
	       (void-element-p :track)
	       (void-element-p :wbr)))))

(define-test t-html
  :parent html-gen-suite
  
  ;; test simple strings
  (is string= "this is a string" (html "this is a string"))
  (is string= "many strings" (html "many"     " "     "stri"    "ngs"))

  ;; test a simple case with no args
  (is string= "<p>a paragraph</p>" (html (:p () "a paragraph")))

  ;; test a simple case with args
  (is string= "<a href=\"http://www.google.com\">google</a>"
      (html (:a ((:href "http://www.google.com")) "google")))

  ;; test nested tags
  (is string= "<p>a paragraph with a link to <a href=\"http://www.google.com\">google</a></p>"
      (html (:p () "a paragraph with a link to "
		(:a ((:href "http://www.google.com")) "google"))))

  ;; test parallell tags
  (is string= "<h1>header</h1><p>paragraph</p>" (html (:h1 () "header")
						      (:p () "paragraph")))

  ;; test a void element (and that is is closed)
  (is string= "<input type=\"text\" value=\"write something\" />"
      (html (:input ((:type "text") (:value "write something")))))

  ;; test no values is printed with void elements
  (is string= "<br />" (html (:br () "this is not printed")))

  ;; nothing should be printed for nil
  (is string= "" (html nil))
  (is string= "<p>foo</p><p>bar</p>" (html (:p () "foo")
					   nil
					   (:p () "bar")))

  ;; test a simple html page
  (is string= "<html><head><title>title</title><meta these=\"are\" meta=\"tags\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"css.css\" /></head><body><div id=\"box\"><h1>header</h1><p>paragraph</p></div><div id=\"foot\"><p>this is a foot</p></div></body></html>"
      (html (:html ()
		   (:head ()
			  (:title () "title")
			  (:meta ((:these "are") (:meta "tags")))
			  (:link ((:rel "stylesheet") (:type "text/css")
						      (:href "css.css"))))
		   (:body ()
			  (:div ((:id "box"))
				(:h1 () "header")
				(:p () "paragraph"))
			  (:div ((:id "foot"))
				(:p () "this is a foot")))))))

(define-test t-generate-html
  :parent html-gen-suite
  
  ;; test the void tags are closed/not closed when
  ;; calling generate-html with different options
  (is string= "<meta these=\"are\" meta=\"tags\" />"
      (with-output-to-string (str)
	(generate-html (str :close-void t)
		       (:meta ((:these "are") (:meta "tags"))))))
  (is string= "<meta these=\"are\" meta=\"tags\" />"
      (with-output-to-string (str)
	(generate-html (str)
		       (:meta ((:these "are") (:meta "tags"))))))
  (is string= "<meta these=\"are\" meta=\"tags\">"
      (with-output-to-string (str)
	(generate-html (str :close-void nil)
		       (:meta ((:these "are") (:meta "tags")))))))

(define-test t-html-variable-and-functions
  :parent html-gen-suite
  (let ((var "variable"))
    (labels ((func (arg) (format nil "~a" arg)))
      (is string= "<p>variable</p>" (html (:p () var)))
      (is string= "<p>function</p>" (html (:p () (func "function")))))))

(define-test t-xml
  :parent html-gen-suite
  (is string= "<meta foo=\"bar\">monkeys eats bananas</meta>"
      (xml (:meta ((:foo "bar")) "monkeys eats bananas"))))

(define-test t-generate-xml
  :parent html-gen-suite
  (is string= "<meta these=\"are\">meta tags</meta>"
      (with-output-to-string (str)
	(generate-xml (str) (:meta ((:these "are")) "meta tags")))))

(define-test t-xml-declaration
  :parent html-gen-suite
  (is string= "<?xml?>" (xml-declaration))
  (is string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      (xml-declaration (:version "1.0") (:encoding "UTF-8"))))
