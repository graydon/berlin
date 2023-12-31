;; $Id: dblink.dsl,v 1.1 1999/08/20 09:13:57 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================= LINKS AND ANCHORS ==========================

(element link
  (let* ((endterm (attribute-string (normalize "endterm")))
	 (linkend (attribute-string (normalize "linkend")))
	 (target  (if endterm
		      (element-with-id endterm)
		      (element-with-id linkend))))
    (if (node-list-empty? target)
	(if endterm
	    (error (string-append "Link to missing ID '" endterm "'"))
	    (process-children))
	(if endterm
	    (make link 
	      destination: (node-list-address (element-with-id endterm))
	      (with-mode xref-endterm-mode (process-element-with-id endterm)))
	    (make link 
	      destination: (node-list-address (element-with-id linkend))
	      (process-children))))))

(element ulink 
  (make sequence
    ($charseq$)
    (if %footnote-ulinks%
	($ss-seq$ + (literal (footnote-number (current-node))))
	(empty-sosofo))))

(element footnoteref 
  (process-element-with-id (attribute-string (normalize "linkend"))))

(element anchor (empty-sosofo))
(element beginpage (empty-sosofo))

;; ======================================================================

(define (olink-link)
  ;; This is an olink without a TARGETDOCENT, treat it as a link within
  ;; the same document.
  (let* ((localinfo (attribute-string (normalize "localinfo")))
	 (target    (element-with-id localinfo))
	 (linkmode  (attribute-string (normalize "linkmode")))
	 (modespec  (if linkmode (element-with-id linkmode) (empty-node-list)))
	 (xreflabel (if (node-list-empty? modespec)
			#f
			(attribute-string (normalize "xreflabel") modespec)))
	 (linktext  (strip (data (current-node)))))
    (if (node-list-empty? target)
	(make sequence
	  (error (string-append "OLink to missing ID '" localinfo "'"))
	  (if (and (equal? linktext "") xreflabel)
	      (literal xreflabel)
	      (process-children)))
	(if (equal? linktext "")
	    (if xreflabel
		(xref-general target xreflabel)
		(xref-general target))
	    (process-children)))))

(define (olink-simple)
  ;; Assumptions: 
  ;; - The TARGETDOCENT is identified by a public ID
  ;; - If the element has no content, the title extracted by
  ;;   (olink-resource-title) should be used
  ;; - The (olink-resource-title) function can deduce the title from
  ;;   the pubid and the sysid
  (let* ((target   (attribute-string (normalize "targetdocent")))
	 (pubid    (entity-public-id target))
	 (sysid    (system-id-filename target))
	 (title    (olink-resource-title pubid sysid))
	 (linktext (strip (data (current-node)))))
    (if (equal? linktext "")
	(make sequence
	  font-posture: 'italic
	  (literal title))
	(process-children))))

(define (olink-outline-xref olroot target linktext)
  (let* ((name  (attribute-string (normalize "name") target))
	 (label (attribute-string (normalize "label") target))
	 (title (select-elements (children target) (normalize "ttl")))
	 (substitute (list
		      (list "%g" (if name (literal name) (literal "")))
		      (list "%n" (if label (literal label) (literal "")))
		      (list "%t" (with-mode olink-title-mode
				   (process-node-list title)))))
	 (tlist   (match-split-list linktext (assoc-objs substitute))))
    (string-list-sosofo tlist substitute)))

(define (olink-outline)
  (let* ((target    (attribute-string (normalize "targetdocent")))
	 (linkmode  (attribute-string (normalize "linkmode")))
	 (localinfo (attribute-string (normalize "localinfo")))
	 (modespec  (if linkmode (element-with-id linkmode) (empty-node-list)))
	 (xreflabel (if (node-list-empty? modespec) 
			""
			(attribute-string (normalize "xreflabel") modespec)))
	 (pubid     (entity-public-id target))
	 (sysid     (system-id-filename target))
	 (basename  (trim-string sysid '(".sgm" ".xml" ".sgml")))
	 (olinkfile (string-append basename %olink-outline-ext%))
	 (olinkdoc  (sgml-parse olinkfile))
	 (olinkroot (node-property 'document-element olinkdoc))
	 (olnode    (if localinfo
			(element-with-id localinfo olinkroot)
			olinkroot))
	 (linktext (strip (data (current-node)))))
    (if (equal? linktext "")
	(olink-outline-xref olinkroot olnode xreflabel)
	(process-children))))

(element olink
  (if (not (attribute-string (normalize "targetdocent")))
      (olink-link)
      (if (attribute-string (normalize "linkmode"))
	  (olink-outline)
	  (olink-simple))))

(mode olink-title-mode
  (default (process-children))

  (element ttl
    (make sequence
      font-posture: 'italic
      (process-children)))

  (element it
    (make sequence
      font-posture: 'upright
      (process-children)))

  (element tt
    (make sequence
      font-family-name: %mono-font-family%
      (process-children)))

  (element sub 
    ($ss-seq$ -))

  (element sup 
    ($ss-seq$ +))
)

;; ======================================================================

(element xref
  (let* ((endterm (attribute-string (normalize "endterm")))
	 (linkend (attribute-string (normalize "linkend")))
	 (target  (element-with-id linkend)))
    (if (node-list-empty? target)
	(error (string-append "XRef LinkEnd to missing ID '" linkend "'"))
	(if endterm
	    (if (node-list-empty? (element-with-id endterm))
		(error (string-append "XRef EndTerm to missing ID '" 
				      endterm "'"))
		(make link 
		  destination: (node-list-address (element-with-id endterm))
		  (with-mode xref-endterm-mode 
		    (process-element-with-id endterm))))
	    (cond
	     ((or (equal? (gi target) (normalize "biblioentry"))
		  (equal? (gi target) (normalize "bibliomixed")))
	      ;; xref to the bibliography is a special case
	      (xref-biblioentry target))
	     ((equal? (gi target) (normalize "co"))
	      ;; callouts are a special case
	      ($callout-mark$ target))
	     (else 
	      (xref-general target)))))))

(define (xref-general target #!optional (xref-string #f))
  ;; This function is used by both XREF and OLINK (when no TARGETDOCENT
  ;; is specified).  The only case where xref-string is supplied is
  ;; on OLINK.
  (let ((label (attribute-string (normalize "xreflabel") target)))
    (make link 
      destination: (node-list-address target)
      (if xref-string
	  (auto-xref target xref-string)
	  (if label
	      (xreflabel-sosofo label)
	      (auto-xref target))))))

(define (xref-biblioentry target)
  (let* ((abbrev (node-list-first 
		  (node-list-filter-out-pis (children target))))
	 (label  (attribute-string (normalize "xreflabel") target)))
    (make link 
      destination: (node-list-address target)

      (if biblio-number 
	  (make sequence
	    (literal "[" (number->string (bibentry-number target)) "]"))
	  (if label
	      (make sequence
		(literal "[" label "]"))
	      (if (equal? (gi abbrev) (normalize "abbrev"))
		  (make sequence
		    (process-node-list abbrev))
		  (make sequence
		    (literal "[" 
			     (attribute-string (normalize "id") target)
			     "]"))))))))

(mode xref-endterm-mode
  (default
    (make sequence
      font-posture: 'italic
      (process-children-trim))))

(define (xreflabel-sosofo xreflabel)
  (make sequence
    font-posture: 'italic
    (literal xreflabel)))

;; ======================================================================

;; Returns the title of the element as a sosofo, italicized for xref.
;;
(define (element-title-xref-sosofo nd)
  (make sequence
    font-posture: 'italic
    (element-title-sosofo nd)))

(mode xref-title-mode
  (element title
    (process-children-trim))

  (element refname
    (process-children-trim))

  (element refentrytitle
    (process-children-trim))
)


;; ======================================================================

(define (element-page-number-sosofo target)
  (with-mode pageno-mode
	(process-node-list target)))

(mode pageno-mode
  (default
    (current-node-page-number-sosofo)))

;; ======================================================================

