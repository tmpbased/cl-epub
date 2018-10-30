(in-package :test)

(define-test classes-suite)

(define-test t-section
  :parent classes-suite
  (with-symbols-from (:html-gen Section section-index section-add-to-toc-p section-title section-paragraphs)
    (let ((s (make-instance 'Section
			    :index 0
			    :add-to-toc-p t
			    :title "title")))
      (is = 0 (section-index s))
      (true (section-add-to-toc-p s))
      (is string= "title" (section-title s))
      (true (hash-table-p (section-paragraphs s))))))

(define-test t-paragraph
  :parent classes-suite
  (with-symbols-from (:html-gen Paragraph paragraph-index paragraph-text)
    (let ((p (make-instance 'Paragraph
			    :index 0
			    :text "<p>foo</p>")))
      (is = 0 (paragraph-index p))
      (is string= "<p>foo</p>" (paragraph-text p)))))

(define-test t-item
  :parent classes-suite
  (with-symbols-from (:html-gen Item item-id item-href item-media-type item-fallback item-properties item-media-overlay)
    (let ((i (make-instance 'Item
			    :id "id"
			    :href "href"
			    :media-type "media-type"
			    :fallback "fallback"
			    :properties "properties"
			    :media-overlay "media-overlay")))
      (is string= "id" (item-id i))
      (is string= "href" (item-href i))
      (is string= "media-type" (item-media-type i))
      (is string= "fallback" (item-fallback i))
      (is string= "properties" (item-properties i))
      (is string= "media-overlay" (item-media-overlay i)))))


(define-test t-itemref
  :parent classes-suite
  (with-symbols-from (:html-gen Itemref itemref-idref itemref-linear itemref-id itemref-properties)
    (let ((i (make-instance 'Itemref
			    :idref "idref"
			    :linear "yes"
			    :id "id"
			    :properties "properties"))) 
      (is string= "idref" (itemref-idref i))
      (is string= "yes" (itemref-linear i))
      (is string= "id" (itemref-id i))
      (is string= "properties" (itemref-properties i)))))

(define-test t-metadata
  :parent classes-suite
  (with-symbols-from (:html-gen Metadata metadata-identifier metadata-title metadata-language
				metadata-modified-timestamp metadata-meta metadata-link metadata-contributor
				metadata-coverage metadata-date metadata-description metadata-format
				metadata-publisher metadata-relation metadata-rights metadata-source
				metadata-subject metadata-type)
    (let ((m (make-instance 'Metadata
			    :identifier "identifier"
			    :title "title"
			    :language "language"
			    :modified-timestamp "modified-timestamp"
			    :meta "meta"
			    :link "link"
			    :contributor "contributor"
			    :coverage "coverage"
			    :date "date"
			    :description "description"
			    :format "format"
			    :publisher "publisher"
			    :relation "relation"
			    :rights "rights"
			    :source "source"
			    :subject "subject"
			    :type "type"))) 
      (is string= "identifier" (metadata-identifier m))
      (is string= "title" (metadata-title m))
      (is string= "language" (metadata-language m))
      (is string= "modified-timestamp" (metadata-modified-timestamp m))
      (is string= "meta" (metadata-meta m))
      (is string= "link" (metadata-link m))
      (is string= "contributor" (metadata-contributor m))
      (is string= "coverage" (metadata-coverage m))
      (is string= "date" (metadata-date m))
      (is string= "description" (metadata-description m))
      (is string= "format" (metadata-format m))
      (is string= "publisher" (metadata-publisher m))
      (is string= "relation" (metadata-relation m))
      (is string= "rights" (metadata-rights m))
      (is string= "source" (metadata-source m))
      (is string= "subject" (metadata-subject m))
      (is string= "type" (metadata-type m)))))

(define-test t-epub
  :parent classes-suite
  (with-symbols-from (:html-gen Epub epub-sections epub-toc epub-manifest epub-metadata Metadata epub-spine)
    (let ((e (make-instance 'Epub
			    :sections '("these are" "the sections"))))
      (is equal '("these are" "the sections") (epub-sections e))
      (true (hash-table-p (epub-toc e)))
      (true (hash-table-p (epub-manifest e)))
      (of-type Metadata (epub-metadata e))
      (false (epub-spine e)))))
