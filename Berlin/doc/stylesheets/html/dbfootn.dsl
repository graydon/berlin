;; $Id: dbfootn.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ======================================================================
;; Handle footnotes in body text

(element footnote ;; A footnote inserts a reference to itself
  (let ((id (if (attribute-string (normalize "id"))
		(attribute-string (normalize "id"))
		(generate-anchor))))
    (make element gi: "A"
	  attributes: (list
		       (list "NAME" id)
		       (list "HREF" (string-append "#FTN." id)))
	  (literal 
	   (string-append 
	    "[" ($footnote-number$ (current-node)) "]")))))
      

(element footnoteref
  (let* ((target (element-with-id (attribute-string (normalize "linkend"))))
	 (id (if (attribute-string (normalize "id") target)
		 (attribute-string (normalize "id") target)
		 (generate-anchor target))))
    (make element gi: "A"
	  attributes: (list
		       (list "HREF" (string-append "#FTN." id)))
	  (literal 
	   (string-append
	    "[" ($footnote-number$ target) "]")))))


(define ($chunk-footnote-number$ footnote)
  (let* ((chunk (chunk-parent footnote))
	 (footnotes (select-elements (descendants chunk) (normalize "footnote"))))
    (let loop ((nl footnotes) (num 1))
      (if (node-list-empty? nl)
	  0
	  (if (node-list=? footnote (node-list-first nl))
	      num
	      (loop (node-list-rest nl)
		    (+ num 1)))))))

(define ($table-footnote-number$ footnote)
  (let* ((chunk (ancestor (normalize "tgroup") footnote))
	 (footnotes (select-elements (descendants chunk) (normalize "footnote"))))
    (let loop ((nl footnotes) (num 1))
      (if (node-list-empty? nl)
	  0
	  (if (node-list=? footnote (node-list-first nl))
	      num
	      (loop (node-list-rest nl)
		    (+ num 1)))))))

(define ($footnote-number$ footnote)
  (if (node-list-empty? (ancestor (normalize "tgroup") footnote))
      (format-number ($chunk-footnote-number$ footnote) "1")
      (format-number ($table-footnote-number$ footnote) "a")))

(mode footnote-mode
  (element footnote
    (process-children))

  (element (footnote para)
    (let ((id (if (attribute-string (normalize "id") (parent (current-node)))
		  (attribute-string (normalize "id") (parent (current-node)))
		  (generate-anchor (parent (current-node))))))
      (make element gi: "P"
	    (if (= (child-number) 1)
		(make sequence
		  (make element gi: "A"
			attributes: (list
				     (list "NAME" (string-append "FTN." id))
				     (list "HREF" (href-to (parent (current-node)))))
			(literal 
			 (string-append "[" 
					($footnote-number$ 
					 (parent (current-node))) 
					"]")))
		  (literal " "))
		(literal ""))
	    (process-children))))
)

(define (non-table-footnotes footnotenl)
  (let loop ((nl footnotenl) (result (empty-node-list)))
    (if (node-list-empty? nl)
	result
	(if (has-ancestor-member? (node-list-first nl) 
				  ($table-element-list$))
	    (loop (node-list-rest nl) 
		  result)
	    (loop (node-list-rest nl)
		  (node-list result (node-list-first nl)))))))

(define (make-endnotes)
  (if %footnotes-at-end%
      (let* ((allfootnotes   (select-elements (descendants (current-node)) 
					      (normalize "footnote")))
	     (allntfootnotes (non-table-footnotes allfootnotes))
	     (this-chunk     (chunk-parent (current-node)))
	     (footnotes      (let loop ((fn allntfootnotes) 
					(chunkfn (empty-node-list)))
			       (if (node-list-empty? fn)
				   chunkfn
				   (if (node-list=? this-chunk
						    (chunk-parent
						     (node-list-first fn)))
				       (loop (node-list-rest fn)
					     (node-list chunkfn 
							(node-list-first fn)))
				       (loop (node-list-rest fn)
					     chunkfn))))))
	(if (node-list-empty? footnotes) 
	    (empty-sosofo)
	    (if (or (equal? (gi (current-node)) (normalize "reference"))
		    (equal? (gi (current-node)) (normalize "part"))
		    (equal? (gi (current-node)) (normalize "set"))
		    (equal? (gi (current-node)) (normalize "book")))
		(empty-sosofo) ;; Each RefEntry/Component does its own...
		(make sequence
		  (make-endnote-header)
		  (make element gi: "TABLE"
			attributes: '(("BORDER" "0")
				      ("CLASS" "FOOTNOTES")
				      ("WIDTH" "100%"))
			(with-mode endnote-mode
			  (process-node-list footnotes)))))))
      (empty-sosofo)))

(define (make-endnote-header)
  (let ((headsize (if (equal? (gi) (normalize "refentry")) "H2" "H3")))
    (make element gi: headsize
	  (literal (gentext-endnotes)))))

(mode endnote-mode
  (element footnote
    (let ((id (if (attribute-string (normalize "id") (current-node))
		  (attribute-string (normalize "id") (current-node))
		  (generate-anchor (current-node)))))
      (make sequence
	(make element gi: "TR"
	      (make element gi: "TD"
		    attributes: '(("ALIGN" "LEFT")
				  ("VALIGN" "TOP")
				  ("WIDTH" "5%"))
		    (make element gi: "A"
			  attributes: (list
				       (list "NAME" (string-append "FTN." id))
				       (list "HREF" (href-to (current-node))))
			  (literal 
			   (string-append "[" 
					  ($footnote-number$ (current-node))
					  "]"))))
	      (make element gi: "TD"
		    attributes: '(("ALIGN" "LEFT")
				  ("VALIGN" "TOP")
				  ("WIDTH" "95%"))
		    (process-children))))))
)

;; ======================================================================
;; Handle table footnotes

(define (table-footnote-number footnote)
  (format-number (component-child-number footnote 
					 (list (normalize "table") 
					       (normalize "informaltable")))
		 "a"))

(element (entry para footnote)
  (make element gi: "SUP"
	(literal (table-footnote-number (current-node)))))

(define (make-table-endnote-header)
  (make sequence
    (literal (gentext-table-endnotes))
    (make empty-element gi: "BR")))

(define (make-table-endnotes)
  (let* ((footnotes (select-elements (descendants (current-node)) 
				     (normalize "footnote")))
	 (tgroup (ancestor-member (current-node) (list (normalize "tgroup"))))
	 (cols   (string->number (attribute-string (normalize "cols") tgroup))))
    (if (node-list-empty? footnotes) 
	(empty-sosofo)
	(make element gi: "TR"
	  (make element gi: "TD"
		attributes: (list 
			     (list "COLSPAN" (number->string cols)))
		(make-table-endnote-header)
		(with-mode table-footnote-mode
		  (process-node-list footnotes)))))))

(mode table-footnote-mode
  (element footnote
    (process-children))

  (element (footnote para)
    (let* ((target (parent (current-node)))
	   (fnnum (table-footnote-number target))
	   (idstr (if (attribute-string (normalize "id") target)
		      (attribute-string (normalize "id") target)
		      (generate-anchor target))))
      (make sequence
	(if (= (child-number) 1)
	    (make element gi: "A"
		  attributes: (list (list "NAME" (string-append "FTN." idstr)))
		  (literal fnnum 
			   (gentext-label-title-sep "FOOTNOTE")))
	    (empty-sosofo))
	(process-children)
	(make empty-element gi: "BR")))))

