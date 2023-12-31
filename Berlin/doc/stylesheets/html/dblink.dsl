;; $Id: dblink.dsl,v 1.1 1999/08/20 09:13:56 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================= LINKS AND ANCHORS ==========================

(element link 
  (let* ((target (element-with-id 
		  (attribute-string (normalize "linkend")))))
    (if (node-list-empty? target)
	(process-children)
	(make element gi: "A"
	      attributes: (list
			   (list "HREF" (href-to target)))
	      (process-children)))))

(element ulink 
  (make element gi: "A"
	attributes: (list
		     (list "HREF" (attribute-string (normalize "url")))
		     (list "TARGET" "_top"))
	(process-children)))

(element anchor
  (make element gi: "A"
	attributes: (list
		     (list "NAME" (attribute-string (normalize "id"))))
	(empty-sosofo)))

(element beginpage (empty-sosofo))

;; ======================================================================

(define (olink-link)
  ;; This is an olink without a TARGETDOCENT, treat it as a link within
  ;; the same document.
  (let* ((localinfo (normalize (attribute-string (normalize "localinfo"))))
	 (target    (element-with-id localinfo))
	 (linkmode  (attribute-string (normalize "linkmode")))
	 (modespec  (if linkmode (element-with-id linkmode) (empty-node-list)))
	 (xreflabel (if (node-list-empty? modespec)
			#f
			(attribute-string (normalize "xreflabel") modespec)))
	 (href      (if (node-list-empty? target)
			(error 
			 (string-append "OLink to missing ID '" localinfo "'"))
			(href-to target)))
	 (linktext  (strip (data (current-node)))))
    (make element gi: "A"
	  attributes: (list (list "HREF" href)
			    (list "CLASS" "OLINK"))
	  (if (equal? linktext "")
	      (if xreflabel
		  (xref-general target xreflabel)
		  (xref-general target))
	      (process-children)))))

(define (olink-href target modespec)
  (let* ((pubid     (entity-public-id target))
	 (sysid     (system-id-filename target))
	 (localinfo (normalize (attribute-string (normalize "localinfo"))))
	 (qident    (if pubid
			(string-append %olink-pubid% (url-encode-string pubid))
			(string-append %olink-sysid% (url-encode-string sysid))))
	 (qfragid   (if localinfo
		        (string-append %olink-fragid% 
				       (url-encode-string localinfo))
		        ""))
	 (lb-href   (string-append %olink-resolution% qident qfragid))
	 (modetext  (if (node-list-empty? modespec) "" (data modespec)))
	 (href      (if (equal? (strip modetext) "")
			lb-href
			(if localinfo
			    (string-append 
			     modetext "#" (url-encode-string localinfo))
			    modetext))))
    href))

(define (olink-simple)
  ;; Assumptions: 
  ;; - The TARGETDOCENT is identified by a public ID
  ;; - LOLCALINFO contains the ID value (i.e. HREF fragment identifier) of
  ;;   the target resource
  ;; - If the element has no content, the title extracted by
  ;;   (olink-resource-title) should be used
  ;; - The (olink-resource-title) function can deduce the title from
  ;;   the pubid and the sysid
  ;; - %olink-resolution% is the prefix to use on URLs (to point to a 
  ;;   cgi-bin script, or whatever you can make work for you)
  ;; - %olink-pubid% identifies the pubid in the query
  ;; - %olink-fragid% identifies the fragment identifier in the query
  (let* ((target   (attribute-string (normalize "targetdocent")))
	 (pubid    (entity-public-id target))
	 (sysid    (system-id-filename target))
	 (title    (olink-resource-title pubid sysid))
	 (href     (olink-href target (empty-node-list)))
	 (linktext (strip (data (current-node)))))
    (make element gi: "A"
	  attributes: (list (list "HREF" href)
			    (list "CLASS" "OLINK"))
	  (if (equal? linktext "")
	      (make element gi: "I" (literal title))
	      (process-children)))))

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
	 (localinfo (normalize (attribute-string (normalize "localinfo"))))
	 (sysid     (entity-generated-system-id target))
	 (basename  (trim-string sysid '(".sgm" ".xml" ".sgml")))
	 (olinkfile (string-append basename %olink-outline-ext%))
	 (olinkdoc  (sgml-parse olinkfile))
	 (olinkroot (node-property 'document-element olinkdoc))
	 (olnode    (if localinfo
			(element-with-id localinfo olinkroot)
			olinkroot))
	 (linkmode  (attribute-string (normalize "linkmode")))
	 (modespec  (if linkmode (element-with-id linkmode) (empty-node-list)))
	 (xreflabel (if (node-list-empty? modespec)
			""
			(attribute-string (normalize "xreflabel") modespec)))
	 (href      (if (equal? (attribute-string (normalize "type")) "href")
			(attribute-string (normalize "href") olnode)
			(olink-href target modespec)))
	 (linktext  (strip (data (current-node)))))
    (make element gi: "A"
	  attributes: (list (list "HREF" href)
			    (list "CLASS" "OLINK"))
	  (if (equal? linktext "")
	      (olink-outline-xref olinkroot olnode xreflabel)
	      (process-children)))))

(element olink
  (if (not (attribute-string (normalize "targetdocent")))
      (olink-link)
      (if (attribute-string (normalize "linkmode"))
	  (olink-outline)
	  (olink-simple))))

(mode olink-title-mode
  (default (process-children))

  (element ttl
    (make element gi: "I"
	  (process-children)))

  (element it
    (make element gi: "I"
	  (process-children)))

  (element tt
    (make element gi: "TT"
	  (process-children)))

  (element sub 
    (make element gi: "SUB"
	  (process-children)))

  (element sup 
    (make element gi: "SUP"
	  (process-children)))
)

;; ======================================================================

(element xref
  (let* ((endterm (attribute-string (normalize "endterm")))
	 (linkend (attribute-string (normalize "linkend")))
	 (target  (element-with-id linkend)))
    (if (node-list-empty? target)
	(error (string-append "XRef LinkEnd to missing ID '" linkend "'"))
	(make element gi: "A"
	      attributes: (list
			   (list "HREF" (href-to target)))
	      (if endterm
		  (if (node-list-empty? (element-with-id endterm))
		      (error (string-append
			      "XRef EndTerm to missing ID '" 
			      endterm "'"))
		      (with-mode xref-endterm-mode
			(process-node-list (element-with-id endterm))))
		  (cond
		   ((or (equal? (gi target) (normalize "biblioentry"))
			(equal? (gi target) (normalize "bibliomixed")))
		    ;; xref to the bibliography is a special case
		    (xref-biblioentry target))
		   ((equal? (gi target) (normalize "co"))
		    ;; callouts are a special case
		    ($callout-mark$ target #f))
		   (else 
		    (xref-general target))))))))

(define (xref-general target #!optional (xref-string #f))
  ;; This function is used by both XREF and OLINK (when no TARGETDOCENT
  ;; is specified).  The only case where xref-string is supplied is
  ;; on OLINK.
  (let ((label (attribute-string (normalize "xreflabel") target)))
    (if xref-string
	(auto-xref target xref-string)
	(if label
	    (xreflabel-sosofo label)
	    (auto-xref target)))))

(define (xref-biblioentry target)
  (let* ((abbrev (node-list-first 
		  (node-list-filter-out-pis (children target))))
	 (label  (attribute-string (normalize "xreflabel") target)))
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
		  (literal "[" (id target) "]")))))))

(mode xref-endterm-mode
  (default
    (make element gi: "I"
	  (process-children-trim))))

(define (xreflabel-sosofo xreflabel)
  (make element gi: "I"
	(literal xreflabel)))

;; Returns the title of the element as a sosofo, italicized for xref.
;;
(define (element-title-xref-sosofo nd)
  (make element gi: "I"
	(element-title-sosofo nd)))

(mode xref-title-mode
  (element title
    (process-children-trim))
  (element refname
    (process-children-trim))
  (element refentrytitle
    (process-children-trim)))

;; ======================================================================

(define (element-page-number-sosofo target)
  (with-mode pageno-mode
	(process-node-list target)))

(mode pageno-mode
  (default
    (current-node-page-number-sosofo)))

;; ======================================================================

