(in-package :test)

(define-test methods-suite)

(define-test t-add-section
  :parent methods-suite
  (with-symbols-from (:html-gen Section Epub add-section epub-sections section-add-to-toc-p
				section-index section-paragraphs section-title)
    (let ((section (make-instance 'Section
				  :index 0
				  :add-to-toc-p nil
				  :title "section title"
				  :paragraphs "this is a paragraph"))
	  (epub (make-instance 'Epub)))
      (add-section epub section)
      ;; the key in the hash table should be 0
      (let ((inserted-section (gethash 0 (epub-sections epub))))
	(is = 0 (section-index inserted-section))
	(is string= "section title" (section-title inserted-section))
	(is string= "this is a paragraph" (section-paragraphs inserted-section))
	(false (section-add-to-toc-p inserted-section))))))

(define-test t-add-paragraph
  :parent methods-suite
  (with-symbols-from (:html-gen Section Paragraph add-paragraph section-paragraphs paragraph-index paragraph-text)
    (let ((section (make-instance 'Section
				  :index 0))
	  (paragraph (make-instance 'Paragraph
				    :index 0
				    :text "<p>this is a paragraph text</p>")))
      (add-paragraph section paragraph)
      ;; the key in the section's hash table should be 0
      (let ((par (gethash 0 (section-paragraphs section))))
	(is = 0 (paragraph-index par))
	(is string= "<p>this is a paragraph text</p>" (paragraph-text par))))))


(define-test t-add-item-to-manifest
  :parent methods-suite
  (with-symbols-from (:html-gen Item Epub add-item-to-manifest epub-manifest item-id item-href item-media-type)
    (let ((item (make-instance 'Item
			       :id "item id"
			       :href "my href"
			       :media-type "text/plain"))
	  (epub (make-instance 'Epub)))
      (add-item-to-manifest epub item)
      ;; the id of the item is the key, ie "item id"
      (let ((itm (table-get "item id" (epub-manifest epub)))) 
	(is string= "item id" (item-id itm))
	(is string= "my href" (item-href itm))
	(is string= "text/plain" (item-media-type itm))))))

(define-test t-add-itemref-to-spine
  :parent methods-suite
  (with-symbols-from (:html-gen Itemref Epub add-itemref-to-spine epub-spine itemref-idref)
    (let ((itemref (make-instance 'Itemref
				  :idref "idref"))
	  (epub (make-instance 'Epub)))
      (add-itemref-to-spine epub itemref)
      (let ((the-itemref (first (epub-spine epub)))) 
	(is string= "idref" (itemref-idref the-itemref))
	;; the itemref should be THE SAME itemref
	(is eq itemref the-itemref)))))

(define-test t-serialize-item-to-html
  :parent methods-suite
  (with-symbols-from (:html-gen Item serialize-to-html)
    (let ((item1 (make-instance 'Item
				:id "item id"
				:href "http://www.example.com"
				:media-type "text/plain"))
	  (item2 (make-instance 'Item
				:id "item2"
				:href "href2"
				:media-type "type2"
				:fallback "fallback"))
	  (item3 (make-instance 'Item
				:id "item3"
				:href "href3"
				:media-type "type3"
				:fallback "fallback"
				:properties "properties"
				:media-overlay "media overlay"))) 
      (is string= "<item id=\"item id\" href=\"http://www.example.com\" media-type=\"text/plain\"></item>"
	  (serialize-to-html item1))
      (is string= "<item id=\"item2\" href=\"href2\" media-type=\"type2\" fallback=\"fallback\"></item>"
	  (serialize-to-html item2))
      (is string= "<item id=\"item3\" href=\"href3\" media-type=\"type3\" fallback=\"fallback\" properties=\"properties\" media-overlay=\"media overlay\"></item>"
	  (serialize-to-html item3)))))

(define-test t-serialize-itemref-to-html
  :parent methods-suite
  (with-symbols-from (:html-gen Itemref serialize-to-html)
    (let ((itemref1 (make-instance 'Itemref :idref "idref"))
	  (itemref2 (make-instance 'Itemref
				   :idref "id2"
				   :linear "yes"))
	  (itemref3 (make-instance 'Itemref
				   :idref "id3"
				   :linear "yes"
				   :id "itemref id"
				   :properties "props")))
      (is string= "<itemref idref=\"idref\"></itemref>"
	  (serialize-to-html itemref1))
      (is string= "<itemref idref=\"id2\" linear=\"yes\"></itemref>"
	  (serialize-to-html itemref2))
      (is string= "<itemref idref=\"id3\" linear=\"yes\" id=\"itemref id\" properties=\"props\"></itemref>"
	  (serialize-to-html itemref3)))))

(define-test t-serialize-metadata-to-html
  :parent methods-suite
  (with-symbols-from (:html-gen Metadata serialize-to-html)
    (let ((m1 (make-instance 'Metadata
			     :identifier "identifier"
			     :title "title"
			     :language "language"
			     :modified-timestamp "today"))
	  (m2 (make-instance 'Metadata
			     :identifier "m2-identifier"
			     :title "m2-title"
			     :language "m2-language"
			     :modified-timestamp "m2-modified-timestamp"
			     :meta "m2-meta"
			     :link "m2-link"
			     :contributor "m2-contributor"
			     :coverage "m2-coverage"
			     :creator "m2-creator"
			     :date "m2-date"
			     :description "m2-description"
			     :format "m2-format"
			     :publisher "m2-publisher"
			     :relation "m2-relation"
			     :rights "m2-rights"
			     :source "m2-source"
			     :subject "m2-subject"
			     :type "m2-type"))
	  (m3 (make-instance 'Metadata
			     :identifier "m3-identifier"
			     :title "m3-title"
			     :language "m3-language"
			     :modified-timestamp "m3-modified-timestamp"
			     :format "m3-format"
			     :publisher "m3-publisher"
			     :source "m3-source"
			     :subject "m3-subject"
			     :type "m3-type"))) 
      (is string= "<metadata><dc:identifier>identifier</dc:identifier><dc:title>title</dc:title><dc:language>language</dc:language><meta property=\"dcterms:modified\">today</meta></metadata>"
	  (serialize-to-html m1))
      (is string= "<metadata><dc:identifier>m2-identifier</dc:identifier><dc:title>m2-title</dc:title><dc:language>m2-language</dc:language><meta property=\"dcterms:modified\">m2-modified-timestamp</meta><meta>m2-meta</meta><link>m2-link</link><dc:contributor>m2-contributor</dc:contributor><dc:coverage>m2-coverage</dc:coverage><dc:creator>m2-creator</dc:creator><dc:date>m2-date</dc:date><dc:description>m2-description</dc:description><dc:format>m2-format</dc:format><dc:publisher>m2-publisher</dc:publisher><dc:relation>m2-relation</dc:relation><dc:rights>m2-rights</dc:rights><dc:source>m2-source</dc:source><dc:subject>m2-subject</dc:subject><dc:type>m2-type</dc:type></metadata>"
	  (serialize-to-html m2))
      (is string= "<metadata><dc:identifier>m3-identifier</dc:identifier><dc:title>m3-title</dc:title><dc:language>m3-language</dc:language><meta property=\"dcterms:modified\">m3-modified-timestamp</meta><dc:format>m3-format</dc:format><dc:publisher>m3-publisher</dc:publisher><dc:source>m3-source</dc:source><dc:subject>m3-subject</dc:subject><dc:type>m3-type</dc:type></metadata>"
	  (serialize-to-html m3)))))

(define-test t-get-ordered-paragraphs
  :parent methods-suite
  (with-symbols-from (:html-gen Paragraph Section add-paragraph get-ordered-paragraphs)
    (let ((p1 (make-instance 'Paragraph
			     :index 0
			     :text "p1"))
	  (p2 (make-instance 'Paragraph
			     :index 1
			     :text "p2"))
	  (p3 (make-instance 'Paragraph
			     :index 2
			     :text "p3"))
	  (section (make-instance 'Section
				  :index 0)))
      (add-paragraph section p2)
      (add-paragraph section p1)
      (add-paragraph section p3)
      (let ((ordered (get-ordered-paragraphs section))) 
	(is eq p1 (first ordered))
	(is eq p2 (second ordered))
	(is eq p3 (third ordered))))))

(define-test t-serialize-section-to-html
  :parent methods-suite
  (with-symbols-from (:html-gen Paragraph Section add-paragraph serialize-to-html)
    (let ((p1 (make-instance 'Paragraph
			     :index 0
			     :text "p1"))
	  (p2 (make-instance 'Paragraph
			     :index 1
			     :text "p2"))
	  (p3 (make-instance 'Paragraph
			     :index 2
			     :text "p3"))
	  (section (make-instance 'Section
				  :index 0)))
      (add-paragraph section p2)
      (add-paragraph section p1)
      (add-paragraph section p3)
      (is string= "<section>p1p2p3</section>"
	  (serialize-to-html section)))))

(define-test t-spine-to-html
  :parent methods-suite
  (with-symbols-from (:html-gen Itemref Epub add-itemref-to-spine spine-to-html)
    (let ((itemref1 (make-instance 'Itemref :idref "idref1"))
	  (itemref2 (make-instance 'Itemref :idref "idref2"))
	  (itemref3 (make-instance 'Itemref :idref "idref3"))
	  (epub (make-instance 'Epub)))
      (add-itemref-to-spine epub itemref1)
      (add-itemref-to-spine epub itemref2)
      (add-itemref-to-spine epub itemref3) 
      (is string= "<spine toc=\"ncx\"><itemref idref=\"idref1\"></itemref><itemref idref=\"idref2\"></itemref><itemref idref=\"idref3\"></itemref></spine>"
	  (spine-to-html epub)))))

(define-test t-write-package-document
  :parent methods-suite
  (with-symbols-from (:html-gen Epub Metadata Item Itemref epub-metadata add-item-to-manifest add-itemref-to-spine write-package-document)
    (let ((epub (make-instance 'Epub))
	  (metadata (make-instance 'Metadata
				   :identifier "identifier"
				   :title "title"
				   :language "language"
				   :modified-timestamp "today"))
	  (i1 (make-instance 'Item
			     :id "id"
			     :href "href"
			     :media-type "media type"))
	  (i2 (make-instance 'Item
			     :id "id2"
			     :href "href2"
			     :media-type "media type2"))
	  (itemref (make-instance 'Itemref :idref "content.xhtml")))
      (add-item-to-manifest epub i1)
      (add-item-to-manifest epub i2)
      (add-itemref-to-spine epub itemref)
      (setf (epub-metadata epub) metadata)
      (write-package-document epub "package-document.opf")
      (with-open-file (stream "package-document.opf") 
	(is string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><package xmlns=\"http//www.idpf.org/2007/opf\" version=\"3.0\" unique-identifier=\"uid\"><metadata><dc:identifier>identifier</dc:identifier><dc:title>title</dc:title><dc:language>language</dc:language><meta property=\"dcterms:modified\">today</meta></metadata><manifest><item id=\"id\" href=\"href\" media-type=\"media type\"></item><item id=\"id2\" href=\"href2\" media-type=\"media type2\"></item></manifest><spine toc=\"ncx\"><itemref idref=\"content.xhtml\"></itemref></spine></package>"
	    (read-line stream))))))

(define-test t-write-container-xml
  :parent methods-suite
  (with-symbols-from (:html-gen write-container-xml)
    (write-container-xml "META-INF/container.xml")
    (with-open-file (stream "META-INF/container.xml")
      (is string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><container xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\" version=\"1.0\"><rootfiles><rootfile full-path=\"Content/package-document.opf\" media-type=\"application/oebps-package+xml\"></rootfile></rootfiles></container>"
	  (read-line stream)))))

(define-test t-write-mimetype
  :parent methods-suite
  (with-symbols-from (:html-gen write-mimetype)
    (write-mimetype "mimetype")
    (with-open-file (stream "mimetype") 
      (is string= "application/epub+zip" (read-line stream)))))

(define-test t-get-ordered-sections
  :parent methods-suite
  (with-symbols-from (:html-gen Section Epub add-section get-ordered-sections)
    (let ((s1 (make-instance 'Section :index 0))
	  (s2 (make-instance 'Section :index 1))
	  (s3 (make-instance 'Section :index 2))
	  (epub (make-instance 'Epub)))
      (add-section epub s3)
      (add-section epub s1)
      (add-section epub s2)
      (let ((sections (get-ordered-sections epub))) 
	(is eq s1 (first sections))
	(is eq s2 (second sections))
	(is eq s3 (third sections))))))

(define-test t-write-nav
  :parent methods-suite
  (with-symbols-from (:html-gen Section Epub add-section write-nav)
    (let ((s1 (make-instance 'Section
			     :index 0
			     :add-to-toc-p t
			     :title "s1"))
	  (s2 (make-instance 'Section
			     :index 1
			     :add-to-toc-p t
			     :title "s2"))
	  (s3 (make-instance 'Section
			     :index 2
			     :add-to-toc-p t
			     :title "s3"))
	  (epub (make-instance 'Epub)))
      (add-section epub s3)
      (add-section epub s1)
      (add-section epub s2)
      (write-nav epub "nav.xhtml")
      (with-open-file (stream "nav.xhtml")
	(is string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:epub=\"http://www.idpf.org/2007/ops\"><head><meta charset=\"utf-8\"></meta></head><body><nav epub:type=\"toc\" id=\"toc\"><ol><li>s1</li><li>s2</li><li>s3</li></ol></nav></body></html>"
	    (read-line stream))))))
