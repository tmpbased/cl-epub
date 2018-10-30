(in-package :test)

(define-test html-gen-suite)

(defmacro with-symbols-from ((package-name &rest symbols) &body body)
  (let ((f-symbols (loop :for s :in symbols :collect 
					    (intern (symbol-name s) package-name))))
    `(progn
       ,@(loop :for symbol :in symbols :for f-symbol :in f-symbols 
	       :with body = body
	       :do (setf body (subst f-symbol symbol body :test #'eq))
	       :finally (return body)))))

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

(deftest t-void-element-p ()
  (check
    ;; check all the void elements are found
    (and (void-element-p :area)
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
	 (void-element-p :wbr))))

(deftest t-html ()
  (check
    ;; test simple strings
    (string= "this is a string" (html "this is a string"))
    (string= "many strings" (html "many"     " "     "stri"    "ngs"))

    ;; test a simple case with no args
    (string= "<p>a paragraph</p>" (html (:p () "a paragraph")))

    ;; test a simple case with args
    (string= "<a href=\"http://www.google.com\">google</a>"
    	     (html (:a ((:href "http://www.google.com")) "google")))

    ;; test nested tags
    (string= "<p>a paragraph with a link to <a href=\"http://www.google.com\">google</a></p>"
    	     (html (:p () "a paragraph with a link to "
		       (:a ((:href "http://www.google.com")) "google"))))

    ;; test parallell tags
    (string= "<h1>header</h1><p>paragraph</p>" (html (:h1 () "header")
						     (:p () "paragraph")))

    ;; test a void element (and that is is closed)
    (string= "<input type=\"text\" value=\"write something\" />"
	     (html (:input ((:type "text") (:value "write something")))))

    ;; test no values is printed with void elements
    (string= "<br />" (html (:br () "this is not printed")))

    ;; nothing should be printed for nil
    (string= "" (html nil))
    (string= "<p>foo</p><p>bar</p>" (html (:p () "foo")
					  nil
					  (:p () "bar")))

    ;; test a simple html page
    (string= "<html><head><title>title</title><meta these=\"are\" meta=\"tags\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"css.css\" /></head><body><div id=\"box\"><h1>header</h1><p>paragraph</p></div><div id=\"foot\"><p>this is a foot</p></div></body></html>"
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
				       (:p () "this is a foot"))))))))

(deftest t-generate-html ()
  (check
    ;; test the void tags are closed/not closed when
    ;; calling generate-html with different options
    (string= "<meta these=\"are\" meta=\"tags\" />"
	     (with-output-to-string (str)
	       (generate-html (str :close-void t)
			      (:meta ((:these "are") (:meta "tags"))))))
    (string= "<meta these=\"are\" meta=\"tags\" />"
	     (with-output-to-string (str)
	       (generate-html (str)
			      (:meta ((:these "are") (:meta "tags"))))))
    (string= "<meta these=\"are\" meta=\"tags\">"
	     (with-output-to-string (str)
	       (generate-html (str :close-void nil)
			      (:meta ((:these "are") (:meta "tags"))))))))

(deftest t-html-variable-and-functions ()
  (let ((var "variable"))
    (labels ((func (arg) (format nil "~a" arg)))
      (check
	(string= "<p>variable</p>" (html (:p () var)))
	(string= "<p>function</p>" (html (:p () (func "function"))))))))

(deftest t-xml ()
  (check
   (string= "<meta foo=\"bar\">monkeys eats bananas</meta>"
	    (xml (:meta ((:foo "bar")) "monkeys eats bananas")))))

(deftest t-generate-xml ()
  (check
   (string= "<meta these=\"are\">meta tags</meta>"
	    (with-output-to-string (str)
	      (generate-xml (str) (:meta ((:these "are")) "meta tags"))))))

(deftest t-xml-declaration ()
  (check
    (string= "<?xml?>" (xml-declaration))
    (string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	     (xml-declaration (:version "1.0") (:encoding "UTF-8")))))

(deftest do-html-gen-tests ()
  (check
    ;; (t-html-statement-p)
    ;; (t-create-attributes-string)
    (t-void-element-p)
    (t-html-variable-and-functions)
    (t-generate-html)
    (t-xml)
    (t-generate-xml)
    (t-xml-declaration)
    (t-html)))
