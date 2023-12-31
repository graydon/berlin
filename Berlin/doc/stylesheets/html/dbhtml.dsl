;; $Id: dbhtml.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define (generate-anchor #!optional (nd (current-node)))
  (string-append "AEN" (number->string (all-element-number nd))))

(define (generate-xptr #!optional (nd (current-node)))
  ;; returns the location of the current node in a modified xptr
  ;; syntax.  This used to be used to calculate unique anchor names
  ;; in the HTML document.  all-element-number seems like a better
  ;; way to go...so this function is probably never called anymore.
  (let loop ((suffix "")
	     (nd nd))
    (let ((eid (id nd)))
      (if eid
	  (string-append "I("
			 eid
			 ")"
			 (if (= (string-length suffix) 0)
			     ""
			     (string-append "C"
					    suffix)))
	  (let ((par (parent nd)))
	    (if (not (node-list-empty? par))
		(loop (string-append "("
				     (number->string (child-number nd))
				     ","
				     (gi nd)
				     ")"
				     suffix)
		      par)
		(string-append (if (= (string-length suffix) 0)
				   "R"
				   "R,C")
			       suffix)))))))

(define (para-check #!optional (place 'stop))
  (let ((inpara (equal? (gi (parent (current-node))) (normalize "para"))))
    (if (and %fix-para-wrappers% inpara)
	(if (equal? place 'stop)
	    (make formatting-instruction data: "&#60;/P>")
	    (make formatting-instruction data: "&#60;P>"))
	(empty-sosofo))))

(define (href-to target)
  ;; Return the HTML HREF for the given node.  If nochunks is true, just
  ;; return the fragment identifier.
  (let* ((id       (element-id target))
	 (curdepth (directory-depth (html-file (current-node))))
	 (entfile  (html-file target))
	 (fragid   (if (chunk? target)
		     ""
		     (string-append "#" id))))
    (if nochunks
	fragid
	(string-append (copy-string "../" curdepth) entfile fragid))))
	
(define (link-target idstring)
  ;; Return the HTML HREF for the given idstring.  For RefEntrys, this is
  ;; just the name of the file, for anything else it's the name of the file
  ;; with the fragment identifier for the specified id.
  (href-to (element-with-id idstring)))

;; ----------------------------------------------------------------------

(define (html-document title-sosofo body-sosofo)
  (let* ((make-entity? (and (not nochunks)
			    (or (chunk?) 
				(node-list=? (current-node) 
					     (sgml-root-element)))))
	 (doc-sosofo 
	  (if make-entity?
	      (make element gi: "HTML"
		    (make element gi: "HEAD"
			  (make element gi: "TITLE" title-sosofo)
			  ($standard-html-header$))
		    (make element gi: "BODY" 
			  attributes: %body-attr%
			  (header-navigation (current-node))
			  body-sosofo
			  (footer-navigation (current-node))))
	      body-sosofo)))
    (if make-entity?
	(make entity
	  system-id: (html-entity-file (html-file))
	  (make document-type
	    name: "HTML"
	    public-id: %html-pubid%)
	  doc-sosofo)
	(if (node-list=? (current-node) (sgml-root-element))
	    (make sequence
	      (make document-type
		name: "HTML"
		public-id: %html-pubid%)
	      doc-sosofo)
	    doc-sosofo))))

;; ----------------------------------------------------------------------

(define ($block-container$)
  (make element gi: "DIV"
	attributes: (list
		     (list "CLASS" (gi)))
	(make element gi: "A"
	      attributes: (list (list "NAME" (element-id)))
	      (empty-sosofo))
	(process-children)))

(define ($paragraph$)
  (let ((footnotes (select-elements (descendants (current-node)) 
				    (normalize "footnote")))
	(tgroup (have-ancestor? (normalize "tgroup"))))
    (make sequence
      (make element gi: "P"
	    attributes: (append
			 (if %default-quadding%
			     (list (list "ALIGN" %default-quadding%))
			     '()))
	    (process-children))
      (if (or %footnotes-at-end% tgroup (node-list-empty? footnotes))
	  (empty-sosofo)
	  (make element gi: "BLOCKQUOTE"
		attributes: (list
			     (list "CLASS" "FOOTNOTES"))
		(with-mode footnote-mode
		  (process-node-list footnotes)))))))

(define ($indent-para-container$)
  (make element gi: "BLOCKQUOTE"
	attributes: (list
		     (list "CLASS" (gi)))
	(process-children)))

(define ($bold-seq$ #!optional (sosofo (process-children)))
  (make element gi: "B"
	attributes: (list
		     (list "CLASS" (gi)))
	sosofo))

(define ($italic-seq$ #!optional (sosofo (process-children)))
  (make element gi: "I"
	attributes: (list
		     (list "CLASS" (gi)))
	sosofo))

(define ($bold-italic-seq$ #!optional (sosofo (process-children)))
  (make element gi: "B"
	attributes: (list
		     (list "CLASS" (gi)))
	(make element gi: "I"
	      sosofo)))

(define ($mono-seq$ #!optional (sosofo (process-children)))
  (make element gi: "TT"
	attributes: (list
		     (list "CLASS" (gi)))
	sosofo))

(define ($italic-mono-seq$ #!optional (sosofo (process-children)))
  (make element gi: "TT"
	attributes: (list
		     (list "CLASS" (gi)))
	(make element gi: "I"
	      sosofo)))

(define ($bold-mono-seq$ #!optional (sosofo (process-children)))
  (make element gi: "TT"
	attributes: (list
		     (list "CLASS" (gi)))
	(make element gi: "B"
	      sosofo)))

(define ($charseq$ #!optional (sosofo (process-children)))
  (make element gi: "SPAN"
	attributes: (list
		     (list "CLASS" (gi)))
	sosofo))

;; ----------------------------------------------------------------------

(define ($standard-html-header$) 
  ;; A hook function to add additional tags to the HEAD of your HTML files
  (let* ((info (info-element))
	 (kws  (select-elements (descendants info) (normalize "keyword")))
	 (home (nav-home (current-node)))
	 (up   (parent (current-node)))
	 (prev (prev-chunk-element (current-node)))
	 (next (next-chunk-element (current-node))))
    (make sequence
      ;; Add the META NAME=GENERATOR tag
      (make empty-element gi: "META"
	    attributes: (list (list "NAME" "GENERATOR")
			      (list "CONTENT" %stylesheet-version%)))

      ;; Add the LINK REV=MAD tag
      (if %link-mailto-url%
	  (make empty-element gi: "LINK"
		attributes: (list (list "REV" "MADE")
				  (list "HREF" %link-mailto-url%)))
	  (empty-sosofo))

      ;; Add the LINK REL=HOME tag
      (if (nav-home? (current-node))
	  (make empty-element gi: "LINK"
		attributes: (list (list "REL" "HOME")
				  (list "TITLE"  (element-title-string home))
				  (list "HREF" (href-to home))))
	  (empty-sosofo))

      ;; Add the LINK REL=UP tag
      (if (nav-up? (current-node))
	  (if (or (node-list-empty? up)
		  (node-list=? up (sgml-root-element)))
	      (empty-sosofo)
	      (make empty-element gi: "LINK"
		    attributes: (list (list "REL" "UP")
				      (list "TITLE"  (element-title-string up))
				      (list "HREF" (href-to up)))))
	  (empty-sosofo))

      ;; Add the LINK REL=PREVIOUS tag
      (if (node-list-empty? prev)
	  (empty-sosofo)
	  (make empty-element gi: "LINK"
		attributes: (list (list "REL" "PREVIOUS")
				  (list "TITLE"  (element-title-string prev))
				  (list "HREF" (href-to prev)))))

      ;; Add the LINK REL=NEXT tag
      (if (node-list-empty? next)
	  (empty-sosofo)
	  (make empty-element gi: "LINK"
		attributes: (list (list "REL" "NEXT")
				  (list "TITLE"  (element-title-string next))
				  (list "HREF" (href-to next)))))

      ;; Add META NAME=KEYWORD tags
      (let loop ((nl kws))
	(if (node-list-empty? nl)
	    (empty-sosofo)
	    (make sequence
	      (make empty-element gi: "META"
		    attributes: (list (list "NAME" "KEYWORD")
				      (list "CONTENT" (data (node-list-first nl)))))
	      (loop (node-list-rest nl)))))

      ;; Add LINK REL=STYLESHEET tag
      (if %stylesheet%
	  (make empty-element gi: "LINK"
		attributes: (list (list "REL" "STYLESHEET")
				  (list "TYPE" %stylesheet-type%)
				  (list "HREF" %stylesheet%)))
	  (empty-sosofo))

      ($user-html-header$ home up prev next))))

(define ($user-html-header$ #!optional 
			    (home (empty-node-list))
			    (up (empty-node-list))
			    (prev (empty-node-list))
			    (next (empty-node-list)))
  ;; Add additional header tags.
  (let loop ((tl %html-header-tags%))
    (if (null? tl)
	(empty-sosofo)
	(make sequence
	  (make empty-element gi: (car (car tl))
		attributes: (cdr (car tl)))
	  (loop (cdr tl))))))

(define ($user-header-navigation$ #!optional 
				  (prev (empty-node-list))
				  (next (empty-node-list))
				  (prevm (empty-node-list))
				  (nextm (empty-node-list)))
  (empty-sosofo))

(define ($user-footer-navigation$ #!optional 
				  (prev (empty-node-list))
				  (next (empty-node-list))
				  (prevm (empty-node-list))
				  (nextm (empty-node-list)))
  (empty-sosofo))

(define ($html-body-start$)
  (empty-sosofo))

(define ($html-body-content-start$)
  (empty-sosofo))

(define ($html-body-content-end$)
  (empty-sosofo))

(define ($html-body-end$)
  (empty-sosofo))
