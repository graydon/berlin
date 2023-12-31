;; $Id: dbblock.dsl,v 1.1 1999/08/20 09:13:56 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(element revhistory ($book-revhistory$))

(element highlights ($block-container$))

(element (para blockquote)
  (make paragraph
    first-line-start-indent: 0pt ;; workaround a bug/feature?
    ;; W/o the preceding line, the first-line-start-indent of the enclosing
    ;; paragraph will apply to the paragraphs in this blockquote which is
    ;; probably not wanted..
    font-size: (* %bf-size% %smaller-size-factor%)
    line-spacing: (* %bf-size% %line-spacing-factor%
		     %smaller-size-factor%)
    space-before: %para-sep%
    start-indent: (+ (inherited-start-indent) 1em)
    end-indent: 1em
    (process-children)))

(element blockquote
  (make paragraph
    font-size: (* %bf-size% %smaller-size-factor%)
    line-spacing: (* %bf-size% %line-spacing-factor%
		     %smaller-size-factor%)
    space-before: %para-sep%
    start-indent: (+ (inherited-start-indent) 1em)
    end-indent: 1em
    (process-children)))

(element epigraph
  (let* ((addln-indent (* %text-width% 0.55))
	 (attrib       (select-elements (children (current-node))
					(normalize "attribution")))
	 (paras        (node-list-filter-by-not-gi
			(children (current-node))
			(list (normalize "attribution")))))
    (make display-group
      start-indent: (+ %body-start-indent% addln-indent)
      font-posture: 'italic
      (process-node-list paras)
      (if (node-list-empty? attrib)
	  (empty-sosofo)
	  (make paragraph
	    space-before: 0pt
	    quadding: 'end
	    (process-node-list attrib))))))

(element attribution
  ($charseq$))

(element (epigraph para)
  (if (absolute-last-sibling? (current-node))
      (make paragraph
	space-before: %para-sep%
	space-after: 0pt
 	quadding: %default-quadding%
	(process-children-trim))
      ($paragraph$)))

(element para ($paragraph$))
(element simpara ($paragraph$))

(element formalpara ($para-container$))

(element (formalpara title) ($runinhead$))
(element (formalpara para) (make sequence (process-children)))

(element sidebar 
  (make box
    display?: #t
    box-type: 'border
    line-thickness: 1pt
    start-indent: (inherited-start-indent)
    end-indent: (inherited-end-indent)
    (if (node-list-empty? (select-elements (children (current-node))
					   (normalize "title")))
	(make display-group
	  start-indent: 2pt
	  end-indent: 2pt
	  space-before: %block-sep%
	  space-after: %block-sep%
	  (process-children))
	(make display-group
	  start-indent: 2pt
	  end-indent: 2pt
	  space-before: 0pt
	  space-after: %block-sep%
	  (make sequence
	    (let* ((object (current-node))
		   (title  (select-elements (children object) 
					    (normalize "title")))
		   (nsep   (gentext-label-title-sep (gi object))))
	      (make paragraph
		font-weight: 'bold
		space-before: %block-sep%
		space-after: %para-sep%
		keep-with-next?: #t
		(literal (gentext-element-name object))
		(if (string=? (element-label object) "")
		    (literal nsep)
		    (literal " " (element-label object) nsep))
		(process-node-list (children title))))
	    (process-children))))))

(element (sidebar title) (empty-sosofo))

(element abstract (process-children))

(element (abstract para) 
  (make paragraph
    space-before: %para-sep%
    space-after: %para-sep%
    start-indent: %body-start-indent%
    (process-children-trim)))

(element authorblurb ($block-container$))

(define ($inline-object$)
  (process-children))

(define ($informal-object$)
  (make display-group
    start-indent: (+ %block-start-indent% (inherited-start-indent))
    space-before: %block-sep%
    space-after: %block-sep%
    (process-children)))

(define (object-title-after #!optional (node (current-node))) 
  (if (member (gi node) ($object-titles-after$))
      #t
      #f))

(define ($formal-object$)
  (let ((object-sosofo (make paragraph
			 space-before: (if (object-title-after)
					   %block-sep%
					   0pt)
			 space-after: (if (object-title-after)
					   0pt
					   %block-sep%)
			 start-indent: (+ %block-start-indent% 
					  (inherited-start-indent))
			 keep-with-next?: (object-title-after)
			 (process-children)))
	(title-sosofo  (with-mode formal-object-title-mode
			 (process-node-list 
			  (select-elements (children (current-node))
					   (normalize "title"))))))
    (if (object-title-after)
	(make sequence
	  object-sosofo
	  title-sosofo)
	(make sequence
	  title-sosofo
	  object-sosofo))))

(define ($semiformal-object$)
  (if (node-list-empty? (select-elements (children (current-node))
					 (normalize "title")))
      ($informal-object$)
      ($formal-object$)))

(mode formal-object-title-mode
  (element title
    (let* ((object (parent (current-node)))
	   (nsep   (gentext-label-title-sep (gi object))))
      (make paragraph
	font-weight: 'bold
	space-before: (if (object-title-after (parent (current-node)))
			  %para-sep%
			  %block-sep%)
	space-after: (if (object-title-after (parent (current-node)))
			  %block-sep%
			  %para-sep%)
	start-indent: (+ %block-start-indent% (inherited-start-indent))
	keep-with-next?: (not (object-title-after (parent (current-node))))
	(literal (gentext-element-name object))
	(if (string=? (element-label object) "")
	    (literal nsep)
	    (literal " " (element-label object) nsep))
	(process-children))))
)

(element example ($formal-object$))

(element (example title) (empty-sosofo)) ; don't show caption below example
(element informalexample ($informal-object$))

(element (figure title) (empty-sosofo)) ; don't show caption below figure

(element figure ($formal-object$))

(element informaltable ($informal-object$))

(element table 
  ;; can't be a "formal-object" because it requires special handling for
  ;; the PGWIDE attribute
  (let* ((nsep   (gentext-label-title-sep (gi)))
	 (pgwide (attribute-string (normalize "pgwide")))
	 (indent (lambda () (if (not (equal? pgwide "1"))
				(+ %block-start-indent% 
				   (inherited-start-indent))
				%cals-pgwide-start-indent%)))
	 (title-sosofo (make paragraph
			 font-weight: 'bold
			 space-before: (if (object-title-after)
					   %para-sep%
					   %block-sep%)
			 space-after: (if (object-title-after)
					  %block-sep%
					  %para-sep%)
			 start-indent: (indent)
			 keep-with-next?: (not (object-title-after))
			 (literal (gentext-element-name (current-node)))
			 (if (string=? (element-label) "")
			     (literal nsep)
			     (literal " " (element-label) nsep))
			 (element-title-sosofo)))
	 (table-sosofo (make paragraph
			 font-weight: 'bold
			 space-before: (if (object-title-after)
					   %block-sep%
					   0pt)
			 space-after: (if (object-title-after)
					   0pt
					   %block-sep%)
			 start-indent: (indent)
			 keep-with-next?: (object-title-after)
			 (process-children))))
    (if (object-title-after)
	(make sequence
	  table-sosofo
	  title-sosofo)
	(make sequence
	  title-sosofo
	  table-sosofo))))

(element (table title) (empty-sosofo))

;; ======================================================================
;; Handle footnotes in the body...

(define %footnote-field-width% 1.6em)
(define %footnote-number-restarts% #t)
(define %footnote-endnote-break% #f)

(define (footnote-number footnote)
  (if %footnote-ulinks% 
      (if %footnote-number-restarts%
	  (format-number (component-list-child-number 
			  footnote 
			  (list (normalize "ulink") (normalize "footnote"))
			  (component-element-list))
			 "1")
	  (format-number (component-list-child-number 
			  footnote 
			  (list (normalize "ulink") (normalize "footnote"))
			  (list (normalize "book")))
			 "1"))
      (if %footnote-number-restarts%
	  (format-number (component-child-number footnote 
						 (component-element-list)) 
			 "1")
	  (format-number (component-child-number footnote 
						 (list (normalize "book")))
			 "1"))))

(element footnote 
  ($ss-seq$ + (literal (footnote-number (current-node)))))

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

(define (make-endnote-header)
  (let ((headsize (if (equal? (gi) (normalize "refentry"))
		      (HSIZE 2)
		      (HSIZE 3)))
	(indent   (lambda () (if (equal? (gi) (normalize "refentry"))
				 %body-start-indent%
				 0pt))))
    (make paragraph
      break-before: %footnote-endnote-break%
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: headsize
      line-spacing: (* headsize %line-spacing-factor%)
      space-before: (* headsize %head-before-factor%)
      space-after: (* headsize %head-after-factor%)
      start-indent: (indent)
      quadding: 'start
      keep-with-next?: #t
      (literal (gentext-endnotes)))))

(define (make-endnotes)
  (let* ((allfootnotes (if %footnote-ulinks%
			   (node-list-filter-by-gi
			    (descendants (current-node))
			    (list (normalize "footnote") (normalize "ulink")))
			   (select-elements (descendants (current-node)) 
					    (normalize "footnote"))))
	 (footnotes    (non-table-footnotes allfootnotes)))
    (if (node-list-empty? footnotes) 
	(empty-sosofo)
	(if (or (equal? (gi (current-node)) (normalize "reference"))
		(equal? (gi (current-node)) (normalize "part")))
	    (empty-sosofo) ;; Each RefEntry/Component does its own...
	    (make sequence
	      (make-endnote-header)
	      (with-mode endnote-mode
		(process-node-list footnotes)))))))

(mode endnote-mode
  (element footnote
    (make sequence
      start-indent: %body-start-indent%
      (process-children)))

  (element (footnote para)
    (let ((fnnum  (footnote-number (parent (current-node)))))
      (if (= (child-number) 1)
	  (make paragraph
	    space-before: %para-sep%
	    start-indent: (+ (inherited-start-indent) %footnote-field-width%)
	    first-line-start-indent: (- %footnote-field-width%)
	    (make line-field
	      field-width: %footnote-field-width%
	      (literal fnnum 
		       (gentext-label-title-sep (normalize "footnote"))))
	    (process-children-trim))
	  (make paragraph
	    start-indent: (+ (inherited-start-indent) %footnote-field-width%)
	    space-before: %para-sep%
	    (process-children-trim)))))

  (element ulink
    ;; this can only get called if %footnote-ulinks% is #t
    (let ((fnnum  (footnote-number (current-node))))
      (make paragraph
	space-before: %para-sep%
	start-indent: (+ (inherited-start-indent) %footnote-field-width%)
	first-line-start-indent: (- %footnote-field-width%)
	(make line-field
	  field-width: %footnote-field-width%
	  (literal fnnum 
		   (gentext-label-title-sep (normalize "footnote"))))
	(literal (attribute-string "url"))))))

;; ======================================================================
;; Handle table footnotes

(define (table-footnote-number footnote)
  (format-number (component-child-number footnote 
					 ($table-element-list$)) "a"))

(element (entry footnote)
  ($ss-seq$ + (literal (table-footnote-number (current-node)))))

(element (entry para footnote)
  ($ss-seq$ + (literal (table-footnote-number (current-node)))))

(define (make-table-endnote-header)
  (make paragraph
    font-family-name: %body-font-family%
    font-weight: 'medium
    font-size: %bf-size%
    start-indent: 0pt
    quadding: 'start
    (literal (gentext-table-endnotes))))

(define (make-table-endnotes)
  (let* ((footnotes (select-elements (descendants (current-node)) 
				     (normalize "footnote")))
	 (headsize (HSIZE 3))
	 (tgroup (ancestor-member (current-node) (list (normalize "tgroup"))))
	 (cols   (string->number (attribute-string (normalize "cols") tgroup))))
    (if (node-list-empty? footnotes) 
	(empty-sosofo)
	(make table-row
	  (make table-cell
	    n-columns-spanned: cols
	    cell-before-row-margin: %cals-cell-before-row-margin%
	    cell-after-row-margin: %cals-cell-after-row-margin%
	    cell-before-column-margin: %cals-cell-before-column-margin%
	    cell-after-column-margin: %cals-cell-after-column-margin%
	    start-indent: %cals-cell-content-start-indent%
	    end-indent: %cals-cell-content-end-indent%
	    (make-table-endnote-header)
	    (with-mode table-footnote-mode
	      (process-node-list footnotes)))))))

(mode table-footnote-mode
  (element footnote
    (make display-group
      font-family-name: %body-font-family%
      font-weight: 'medium
      font-size: %bf-size%
      start-indent: 0pt
      quadding: 'start
      (process-children)))

  (element (footnote para)
    (let ((fnnum (table-footnote-number (parent (current-node)))))
      (if (= (child-number) 1)
	  (make paragraph
	    start-indent: %footnote-field-width%
	    first-line-start-indent: (- %footnote-field-width%)
	    (make line-field
	      field-width: %footnote-field-width%
	      (literal fnnum 
		       (gentext-label-title-sep (normalize "footnote"))))
	    (process-children-trim))
	  (make paragraph
	    start-indent: %footnote-field-width%
	    (process-children-trim))))))

