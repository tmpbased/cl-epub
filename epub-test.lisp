(in-package :test)

(defun test-cleanup ()
  (with-symbols-from (:epub *container-xml-path* *package-document-path* *content-path* *nav-path*)
    (dolist (file (list *container-xml-path* *package-document-path*
			*content-path* *nav-path*))
      (delete-file file))))

(define-test epub-suite)

(define-test t-write-mimetype
  :parent epub-suite
  (write-mimetype)
  (with-open-file (stream "mimetype")
    (is string= "application/epub+zip" (read-line stream))))

(define-test t-write-container-xml
  :parent epub-suite
  (write-container-xml)
  (with-open-file (stream "META-INF/container.xml")
    (is string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><container xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\" version=\"1.0\"><rootfiles><rootfile full-path=\"Content/package-document.opf\" media-type=\"application/oebps-package+xml\"></rootfile></rootfiles></container>"
	(read-line stream))))

(define-test t-write-content
  :parent epub-suite
  (with-symbols-from (:epub Paragraph Section *book-title* *sections* *content-path*)
    (let* ((*book-title* "this is a book title")
	   (p1 (make-instance 'Paragraph :id 1 :text (html (:p () "p1"))))
	   (p2 (make-instance 'Paragraph :id 2 :text (html (:p () "p2"))))
	   (p3 (make-instance 'Paragraph :id 3 :text (html (:p () "p3"))))
	   (p4 (make-instance 'Paragraph :id 4 :text (html (:p () "p4"))))
	   (*sections* (list (make-instance 'Section
					    :id 0
					    :paragraphs (list p1 p2))
			     (make-instance 'Section
					    :id 1
					    :paragraphs (list p3 p4)))))
      (write-content)
      (with-open-file (stream *content-path*)
	(is string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" xmlns:epub=\"http://www.idpf.org/2007/ops\"><head><meta charset=\"utf-8\"></meta><title>this is a book title</title><link rel=\"stylesheet\" type=\"text/css\" href=\"css.css\"></link></head><body><section id=\"s0\"><p>p1</p><p>p2</p></section><section id=\"s1\"><p>p3</p><p>p4</p></section></body></html>"
	    (read-line stream))))))

(define-test t-write-package-document
  :parent epub-suite
  (let ((metadata (xml (:metadata () "this is metadata")))
	(manifest (xml (:manifest () "this is manifest")))
	(spine (xml (:spine () "this is spine")))
	(guide (xml (:guide () "this is a guide")))
	(bindings (xml (:bindings () "these are bindings"))))
    (with-symbols-from (:epub *package-document-path*)
      (progn
	(write-package-document metadata manifest spine)
	(with-open-file (stream *package-document-path*)
	  (is string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><package xmlns=\"http://www.idpf.org/2007/opf\" version=\"3.0\" unique-identifier=\"uid\"><metadata>this is metadata</metadata><manifest>this is manifest</manifest><spine>this is spine</spine></package>"
	      (read-line stream))))
      (progn
	(write-package-document metadata manifest spine guide bindings)
	(with-open-file (stream *package-document-path*)
	  (is string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><package xmlns=\"http://www.idpf.org/2007/opf\" version=\"3.0\" unique-identifier=\"uid\"><metadata>this is metadata</metadata><manifest>this is manifest</manifest><spine>this is spine</spine><guide>this is a guide</guide><bindings>these are bindings</bindings></package>"
	      (read-line stream)))))))


(define-test t-make-keyword
  :parent epub-suite
  (is eql :foo (make-keyword 'foo))
  (is eql :|foo| (make-keyword "foo")))

(define-test t-parameters
  :parent epub-suite
  (with-symbols-from (:epub *package-document-path* *content-path* *nav-id* *nav-path*)
    (is string= "Content/package-document.opf" *package-document-path*)
    (is string= "Content/content.xhtml" *content-path*)
    (is string= "nav" *nav-id*)
    (is string= "Content/nav.xhtml" *nav-path*)))

(define-test t-create-metadata
  :parent epub-suite
  (let ((identifier "let-identifier")
	(title "let-title")
	(language "let-language")
	(modified "today")
	(meta "let-meta")
	(link "let-link")
	(contributor "let-contributor")
	(coverage "let-coverage")
	(creator "let-creator")
	(date "let-date")
	(description "let-description")
	(format "let-format")
	(publisher "let-publisher")
	(relation "let-relation")
	(rights "let-rights")
	(source "let-source")
	(object "let-object")
	(type "let-type"))
    ;; try all the arguments
    (is string= "<metadata><dc:identifier id=\"uid\">let-identifier</dc:identifier><meta property=\"dcterms:modified\">today</meta><dc:title>let-title</dc:title><dc:language>let-language</dc:language><meta>let-meta</meta><link>let-link</link><dc:contributor>let-contributor</dc:contributor><dc:coverage>let-coverage</dc:coverage><dc:creator>let-creator</dc:creator><dc:date>let-date</dc:date><dc:description>let-description</dc:description><dc:format>let-format</dc:format><dc:publisher>let-publisher</dc:publisher><dc:relation>let-relation</dc:relation><dc:rights>let-rights</dc:rights><dc:source>let-source</dc:source><dc:object>let-object</dc:object><dc:type>let-type</dc:type></metadata>"
	(create-metadata identifier title language modified
			 :meta meta :link link :contributor contributor
			 :coverage coverage :creator creator :date date
			 :description description :format format
			 :publisher publisher :relation relation
			 :rights rights :source source :object object
			 :type type))
    ;; try some arguments
    (is string= "<metadata><dc:identifier id=\"uid\">let-identifier</dc:identifier><meta property=\"dcterms:modified\">today</meta><dc:title>let-title</dc:title><dc:language>let-language</dc:language><meta>let-meta</meta><dc:contributor>let-contributor</dc:contributor></metadata>"
	(create-metadata identifier title language modified
			 :meta meta :contributor contributor))))

(define-test t-create-item
  :parent epub-suite
  (with-symbols-from (:epub create-item)
    ;; check with the required args
    (is string= "<item id=\"id\" href=\"my href\" media-type=\"media type\"></item>"
	(create-item "id" "my href" "media type"))

    ;; check with an optional arg
    (is string= "<item id=\"id\" href=\"my href\" media-type=\"media type\" properties=\"props\"></item>"
	(create-item "id" "my href" "media type" :properties "props"))

    ;; test with all args
    (is string= "<item id=\"id\" href=\"my href\" media-type=\"media type\" fallback=\"fall\" properties=\"props\" media-overlay=\"media\"></item>"
	(create-item "id" "my href" "media type"
		     :fallback "fall" :properties "props"
		     :media-overlay "media"))))

(define-test t-create-manifest
  :parent epub-suite
  (with-symbols-from (:epub create-manifest)
    (is string= "<manifest><item id=\"nav\" href=\"nav.xhtml\" media-type=\"application/xhtml+xml\" properties=\"nav\"></item></manifest>"
	(create-manifest))
    (is string= "<manifest><item id=\"nav\" href=\"nav.xhtml\" media-type=\"application/xhtml+xml\" properties=\"nav\"></item>i1i2</manifest>"
	(create-manifest "i1" "i2"))))

(define-test t-create-spine
  :parent epub-suite
  (with-symbols-from (:epub create-spine)
    (is string= "<spine></spine>" (create-spine))
    (is string= "<spine>i1i2</spine>" (create-spine "i1" "i2"))))

(define-test t-create-itemref
  :parent epub-suite
  (with-symbols-from (:epub create-itemref)
    (is string= "<itemref idref=\"ref\"></itemref>" (create-itemref "ref"))
    (is string= "<itemref idref=\"ref\" linear=\"no\"></itemref>"
	(create-itemref "ref" :linear "no"))
    (is string= "<itemref idref=\"ref\" linear=\"no\" id=\"id\" properties=\"props\"></itemref>"
	(create-itemref "ref" :linear "no" :id "id" :properties "props"))))

(define-test t-section
  :parent epub-suite
  (with-symbols-from (:epub Section section-id section-add-to-toc section-paragraphs)
    ;; TODO paragraphs shouldn't be strings
    (let ((s1 (make-instance 'Section :id "id"
				      :paragraphs '("these are" "my paragraphs")))
	  (s2 (make-instance 'Section :id "id2" :add-to-toc t
				      :paragraphs '("my paragraphs" "are short"))))
      
      ;; test the readers of the Section class
      (is string= "id" (section-id s1))
      (false (section-add-to-toc s1))
      (true (section-add-to-toc s2))
      (is equal '("these are" "my paragraphs") (section-paragraphs s1)))))

(define-test t-defsection
  :parent epub-suite
  (with-symbols-from (:epub *sections* defsection section-id section-add-to-toc section-paragraphs)
    ;; test the defsection macro works
    (let ((*sections* nil))
      ;; TODO paragraphs shouldn't be strings
      (defsection ("my id" :add-to-toc t) "these" "are" "my" "paragraphs")
      
      ;; test the length of *sections* is 1
      (false (cdr *sections*))

      ;; test the fields of the section
      (is string= "my id" (section-id (car *sections*)))
      (true (section-add-to-toc (car *sections*)))
      (is equal '("these" "are" "my" "paragraphs")
	  (section-paragraphs (car *sections*))))))

(define-test t-section-to-html
  :parent epub-suite
  (with-symbols-from (:epub Section *sections* section-to-html)
    (let ((*sections* nil)
	  ;; TODO paragraphs shouldn't be strings
	  (sec (make-instance 'Section :id "my id" :add-to-toc t :paragraphs '("this is my sentence " "and some more words")))) 
      (is string= "<section id=\"smy id\">this is my sentence and some more words</section>"
	  (section-to-html sec)))))

(define-test t-add-paragraph-to-section
  :parent epub-suite
  (with-symbols-from (:epub Section Paragraph add-paragraph-to-section paragraph-text section-paragraphs)
    (let ((s (make-instance 'Section :id "id" :paragraphs nil))
	  (p1 (make-instance 'Paragraph :id 0 :text "one"))
	  (p2 (make-instance 'Paragraph :id 1 :text "two"))
	  (p3 (make-instance 'Paragraph :id 3 :text "three")))
      (add-paragraph-to-section s p3)
      (add-paragraph-to-section s p2)
      (add-paragraph-to-section s p1)
      (is string= "one" (paragraph-text (first (section-paragraphs s))))
      (is string= "two" (paragraph-text (second (section-paragraphs s))))
      (is string= "three" (paragraph-text (third (section-paragraphs s)))))))

(define-test t-get-sorted-paragraphs
  :parent epub-suite
  (with-symbols-from (:epub Section Paragraph add-paragraph-to-section paragraph-text get-sorted-paragraphs)
    (let ((s (make-instance 'Section :id "id" :paragraphs nil)))
      (add-paragraph-to-section s (make-instance 'Paragraph :id 2 :text "2"))
      (add-paragraph-to-section s (make-instance 'Paragraph :id 0 :text "0"))
      (add-paragraph-to-section s (make-instance 'Paragraph :id 1 :text "1"))
      (add-paragraph-to-section s (make-instance 'Paragraph :id 3 :text "3"))
      (let ((sorted (mapcar #'(lambda (p)	(paragraph-text p))
			    (get-sorted-paragraphs s))))
	(is string= "0" (first sorted))
	(is string= "1" (second sorted))
	(is string= "2" (third sorted))
	(is string= "3" (fourth sorted))))))
