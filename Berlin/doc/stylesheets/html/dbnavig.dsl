;; $Id: dbnavig.dsl,v 1.1 1999/08/20 09:13:56 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define (chunk-element-list)
  (list (normalize "preface")
	(normalize "chapter")
	(normalize "appendix") 
	(normalize "article")
	(normalize "glossary")
	(normalize "bibliography")
	(normalize "index")
	(normalize "setindex")
	(normalize "reference")
	(normalize "refentry")
	(normalize "part")
	(normalize "sect1") 
	(normalize "book") ;; just in case nothing else matches...
	(normalize "set")  ;; sets are definitely chunks...
	))

(define (chunk-skip-first-element-list)
  (list (normalize "sect1")))

(define (subset testlist memberlist)
  ;; Returns #t if all the elements of testlist are also elements of memberlist
  (let loop ((l testlist))
    (if (null? l)
	#t
	(if (not (member (car l) memberlist))
	    #f
	    (loop (cdr l))))))

(define (nodelist-to-gilist nodelist) 
  (let loop ((nl nodelist) (gilist '()))
    (if (node-list-empty? nl)
	gilist
	(loop (node-list-rest nl) (append gilist (list (gi (node-list-first nl))))))))

(define (x-is-first-element nd)
  ;; Returns #t if nd is the first non-meta element of it's parent

;; This doesn't work at all!  If there's a PARA before the first
;; sect1, then it doesn't think that the sect1 is the first element,
;; but for the purposes that are required, it is.  I think
;; this should just be child-number.  Why did I think otherwise?
;; Was I just confused about what child-number returned?

  (let ((metaelements (list (normalize "title")
			    (normalize "bookinfo")
			    (normalize "docinfo")
			    (normalize "sect1info")
			    (normalize "sect2info")
			    (normalize "sect3info") 
			    (normalize "sect4info")
			    (normalize "sect5info"))))
    (and (not (member (gi nd) metaelements))
	 (subset (nodelist-to-gilist (preced nd)) metaelements))))

(define (is-first-element nd)
  (equal? (child-number nd) 1))

(define (combined-chunk? #!optional (nd (current-node)))
  (or
   ;; if it's the first skipped chunk in a chunk
   (and (not (node-list-empty? nd))
	(member (gi nd) (chunk-element-list))
	(is-first-element nd)
	(member (gi nd) (chunk-skip-first-element-list)))
   ;; of if it's a chunk in a partintro
   (and (member (gi nd) (chunk-element-list))
	(has-ancestor-member? nd (list (normalize "partintro"))))))

(define (chunk? #!optional (nd (current-node)))
  ;; A node is not a chunk if
  ;; 1. nochunks is true, or
  ;; 2. chunks='no' was specified on the dbhtml PI at the root node
  ;;    and this is not the root node (i.e., the root node is always
  ;;    a chunk if we're chunking.  Otherwise the root filename isn't
  ;;    calculated correctly.
  ;;-
  (let ((nochunk (or nochunks
		     (and (not (node-list=? nd (sgml-root-element)))
			  (equal? (dbhtml-value 
				   (sgml-root-element) "chunk") "no")))))
    (if nochunk
	#f ;; (node-list=? nd (sgml-root-element))
	(if (member (gi nd) (chunk-element-list))
	    (if (combined-chunk? nd)
		#f
		#t)
	    #f))))

(define (nav-banner? elemnode)
  (if (node-list=? elemnode (sgml-root-element))
      #f
      #t))

;;(define (nav-banner elemnode)
;;  (if (node-list=? elemnode (sgml-root-element))
;;      (literal "TITLE BANNER")
;;      (literal "BANNER")))

(define (nav-banner elemnode)
  (let ((root (sgml-root-element)))
    (element-title-sosofo root)))

(define (nav-home? elemnode)
  (not (node-list=? elemnode (sgml-root-element))))

(define (nav-home elemnode)
  (sgml-root-element))

(define (nav-home-link elemnode)
  (let ((home (nav-home elemnode)))
    (if (node-list=? elemnode home)
	(make entity-ref name: "nbsp")
	(make element gi: "A"
	      attributes: (list
			   (list "HREF" 
				 (href-to home)))
	      (gentext-nav-home home)))))

(define (nav-footer elemnode)
  (empty-sosofo))

;; nav-up is displayed in the bottom center of the footer-navigation
;; table.  The definition below will show "Up" for nested components
;; (the component wrapping a section, the division wrapping a component
;; etc.).  It can be abused for other things, such as an index...
;;
(define (nav-up? elemnode)
  (let ((up (parent elemnode)))
    (if (or (node-list-empty? up)
	    (node-list=? up (sgml-root-element))
	    (equal? (gi up) (normalize "bookinfo"))
	    (equal? (gi up) (normalize "docinfo"))
	    (equal? (gi up) (normalize "setinfo")))
	#f
	#t)))

(define (nav-up elemnode)
  (let ((up (parent elemnode)))
    (if (or (node-list-empty? up)
	    (node-list=? up (sgml-root-element)))
	(make entity-ref name: "nbsp")
	(make element gi: "A"
	      attributes: (list
			   (list "HREF" (href-to up)))
	      (gentext-nav-up up)))))

(define (nav-context? elemnode)
  ;; Print a context header iff, the chunk is a sect1 and the parent
  ;; component isn't the same as the root element (which appears in
  ;; the nav-banner.
  (let ((component (ancestor-member elemnode
				    (append (book-element-list)
					    (division-element-list)
					    (component-element-list))))
	(rootelem (sgml-root-element)))
    (and (equal? (gi elemnode) (normalize "sect1"))
	 (not (node-list=? component rootelem)))))

(define (nav-context elemnode)
  (let* ((component (ancestor-member elemnode
				      (append (book-element-list)
					      (division-element-list)
					      (component-element-list))))
	 (num       (if (node-list-empty? component)
			0
			(element-number component))))
    (if (nav-context? elemnode)
	(if (equal? (element-label component) "")
	    (make sequence
	      (element-title-sosofo component))
	    (make sequence
	      ;; Special case.  This is a bit of a hack.  I need to revisit
	      ;; this aspect of appendixes. 
	      (if (and (equal? (gi component) (normalize "appendix"))
		       (equal? (gi elemnode) (normalize "sect1"))
		       (equal? (gi (parent component)) (normalize "article")))
		  (empty-sosofo)
		  (literal (gentext-element-name-space (gi component))))
	      (element-label-sosofo component)
	      (literal (gentext-label-title-sep (gi component)))
	      (element-title-sosofo component)))
	(empty-sosofo))))

(define (element-id #!optional (nd (current-node)))
  ;; IDs of TITLEs are the IDs of the PARENTs
  (let ((elem (if (equal? (gi nd)
			  (normalize "title"))
		  (parent nd)
		  nd)))
    (if (attribute-string (normalize "id") elem)
	(attribute-string (normalize "id") elem)
	(generate-anchor elem))))
  
(define (pi-value component piname)
  ;; Returns the value of the (?piname value) PI (if one exists)
  ;; as a child of component, otherwise returns #f
  ;;
  (let loop ((nl (select-by-class (children component) 'pi)))
    (if (node-list-empty? nl)
	#f
	(let ((pidata (node-property 'system-data (node-list-first nl))))
	  (if (and (> (string-length pidata) (string-length piname))
		   (equal? piname
			   (substring pidata 0 (string-length piname))))
	      (substring pidata
			 (+ (string-length piname) 1)
			 (string-length pidata))
	      (loop (node-list-rest nl)))))))

(define (inherited-pi-value component piname)
  (let loop ((value #f) (nd component))
    (if (or value (node-list-empty? nd))
	value
	(loop (pi-value nd piname) (parent nd)))))

(define (dbhtml-findvalue pi-field-list name)
  ;; pi-field-list is '(pitarget name1 value1 name2 value2 ...)
  (let loop ((slist (cdr pi-field-list)))
    (if (null? slist)
	#f
	(if (string=? (car slist) name)
	    (car (cdr slist))
	    (loop (cdr (cdr slist)))))))

(define (dbhtml-value component name)
  ;; Returns the value of "name='value'" in the &#60;?dbhtml ...> PI
  (let loop ((nl (select-by-class (children component) 'pi)))
    (if (node-list-empty? nl)
	#f
	(let* ((pidata (node-property 'system-data (node-list-first nl)))
	       (pilist (if (and (> (string-length pidata) 7)
				(string=? (substring pidata 0 7) "dbhtml "))
			   (parse-starttag-pi pidata)
			   '()))
	       (value  (if (null? pilist) #f (dbhtml-findvalue pilist name))))
	  (if value
	      value
	      (loop (node-list-rest nl)))))))

(define (inherited-dbhtml-value component name)
  (let loop ((value #f) (nd component))
    (if (or value (node-list-empty? nd))
	value
	(loop (dbhtml-value nd name) (parent nd)))))

(define (book-html-base nd)
  (let ((number (number->string (all-element-number nd)))
	;(number (pad-string (number->string 3) 2 "0"))
	(prefix (inherited-dbhtml-value nd "prefix"))
	(pibase (or
		 (inherited-dbhtml-value nd "basename")
		 (inherited-pi-value nd "html-basename")))
	(idbase (if (and %use-id-as-filename% 
			 (attribute-string (normalize "id") nd))
		    (case-fold-down (attribute-string (normalize "id") nd))
		    #f)))
    (if idbase
	(string-append (if prefix prefix "") idbase)
	(string-append (if prefix prefix "") 
		       (if pibase pibase "book") number))))

(define (division-html-base nd)
  (let* ((number (number->string (all-element-number nd)))
	 (prefix (inherited-dbhtml-value nd "prefix"))
	 (pibase (or
		  (inherited-dbhtml-value nd "basename")
		  (inherited-pi-value nd "html-basename")))
	 (idbase (if (and %use-id-as-filename% 
			  (attribute-string (normalize "id") nd))
		     (case-fold-down (attribute-string (normalize "id") nd))
		     #f))
	 (base   (cond (pibase pibase)
		       (idbase idbase)
		       ((equal? (gi nd) (normalize "set"))          "s")
		       ((equal? (gi nd) (normalize "preface"))      "f")
		       ((equal? (gi nd) (normalize "chapter"))      "c")
		       ((equal? (gi nd) (normalize "article"))      "t")
		       ((equal? (gi nd) (normalize "appendix"))     "a")
		       ((equal? (gi nd) (normalize "part"))         "p")
		       ((equal? (gi nd) (normalize "reference"))    "r")
		       ((equal? (gi nd) (normalize "glossary"))     "g")
		       ((equal? (gi nd) (normalize "bibliography")) "b")
		       ((equal? (gi nd) (normalize "index"))        "i")
		       ((equal? (gi nd) (normalize "setindex"))     "n")
		       ((equal? (gi nd) (normalize "refentry"))     "r")
		       ;; "x" is section
		       (else "z"))))
    (if idbase
	(string-append (if prefix prefix "") idbase)
	(if pibase
	    (string-append (if prefix prefix "") pibase number)
	    (string-append (if prefix prefix "") base number)))))

(define (component-html-base nd)
  (division-html-base nd))

(define (section-html-base nd)
  ;; Now that I'm using all-element-number, there's no point in basing
  ;; it off the component-html-base at all...
  (let* ((number (number->string (all-element-number nd)))
	 (prefix (inherited-dbhtml-value nd "prefix"))
	 (pibase (or
		  (inherited-dbhtml-value nd "basename")
		  (inherited-pi-value nd "html-basename")))
	 (idbase (if (and %use-id-as-filename% 
			  (attribute-string (normalize "id") nd))
		     (case-fold-down (attribute-string (normalize "id") nd))
		     #f))
	 (base   (if pibase
		     (string-append (if prefix prefix "") pibase)
		     (string-append (if prefix prefix "") "x"))))
    (if idbase
	(string-append (if prefix prefix "") idbase)
	(if (chunk? nd)
	    (string-append base number)
	    base))))

(define (element-html-base nd)
  (let* ((number (number->string (all-element-number nd)))
	 (prefix (inherited-dbhtml-value nd "prefix"))
	 (pibase (or
		  (inherited-dbhtml-value nd "basename")
		  (inherited-pi-value nd "html-basename")))
	 (idbase (if (and %use-id-as-filename% 
			  (attribute-string (normalize "id") nd))
		     (case-fold-down (attribute-string (normalize "id") nd))
		     #f))
	 (base   (if pibase
		     (string-append (if prefix prefix "") pibase)
		     (string-append (if prefix prefix "") 
				    (case-fold-down (gi nd))))))
    (if idbase
	(string-append (if prefix prefix "") idbase)
	(string-append base number))))

;; Returns the filename of the html file that contains elemnode
;;
(define (html-file #!optional (input_nd (current-node)))
  (let* ((nd (chunk-parent input_nd))
	 (base (cond ((member (gi nd) (book-element-list))
		      (book-html-base nd))
		     ((member (gi nd) (division-element-list))
		      (division-html-base nd))
		     ((member (gi nd) (component-element-list))
		      (component-html-base nd))
		     ((member (gi nd) (section-element-list))
		      (section-html-base nd))
		     (else (element-html-base input_nd))))
	 ;; If this chunk-level element isn't a chunk, get the pifile from
	 ;; the parent element.
	 (pifile (if (chunk? nd)
		     (or
		      (dbhtml-value nd "filename")
		      (pi-value nd "html-filename"))
		     (or
		      (dbhtml-value (parent nd) "filename")
		      (pi-value (parent nd) "html-filename"))))
	 (pidir (or
		 (inherited-dbhtml-value nd "dir")
     		 (inherited-pi-value nd "html-dir"))))
    (if (and %root-filename% (node-list=? (sgml-root-element) nd))
	(string-append %root-filename% %html-ext%)
	(if pifile 
	    (if pidir
		(string-append pidir "/" pifile)
		pifile)
	    (if pidir
		(string-append pidir "/" base %html-ext%)
		(string-append base %html-ext%))))))

(define use-output-dir #f)
(define (html-entity-file htmlfilename)
  ;; Returns the filename that should be used for _writing_ htmlfilename.
  ;; This may differ from the filename used in referencing it.  (The point
  ;; is that you can force the stylesheets to write the chunked files
  ;; somewhere else, if you want.)
  (let ((outputdir (dbhtml-value (sgml-root-element) "output-dir")))
    (if (and use-output-dir outputdir)
	(string-append outputdir "/" htmlfilename)
	htmlfilename)))
  
(define (component-level-parent nd)
  (ancestor-member nd (component-element-list)))

(define (root-rel-path filename #!optional (node (current-node)))
  ;; Return the filename relative to the root path
  (string-append (copy-string "../" (directory-depth (html-file node)))
		 filename))

;; Split node list nl at nd; return '(nodes-prev-to-nd nodes-following-nd)
;; Note that nd does not appear in either return list.
(define (split-node-list nd nodelist)
  (let loop ((prev (empty-node-list)) 
	     (nl nodelist))
    (if (node-list-empty? nl)
	(list prev (empty-node-list))
	(if (node-list=? (node-list-first nl) nd)
	    (list prev (node-list-rest nl))
	    (loop (node-list prev (node-list-first nl))
		  (node-list-rest nl))))))

(define (chunk-parent #!optional (nd (current-node)))
  (let loop ((p (chunk-level-parent nd)))
    (if (or (node-list-empty? p) (chunk? p))
	p
	(chunk-level-parent (parent p)))))

(define (chunk-level-parent #!optional (nd (current-node)))
  (ancestor-member nd (chunk-element-list)))

(define (chunk-children #!optional (nd (current-node)))
  (node-list-filter-by-gi (children nd) (chunk-element-list)))

(define (ifollow-by-gi nd gilist)
  (let loop ((next (ifollow nd)))
    (if (node-list-empty? next)
	(empty-node-list)
	(if (member (gi next) gilist)
	    next
	    (loop (ifollow next))))))

(define (ipreced-by-gi nd gilist)
  (let loop ((prev (ipreced nd)))
    (if (node-list-empty? prev)
	(empty-node-list)
	(if (member (gi prev) gilist)
	    prev
	    (loop (ipreced prev))))))

(define (navigate-to? nd)
  #t)

(define (last-chunk-element nd)
  (let ((clc (node-list-filter-by-gi (children nd) (chunk-element-list))))
    (if (node-list-empty? clc)
	nd
	(last-chunk-element (node-list-last clc)))))

(define (next-chunk-skip-children #!optional (elem (current-node)))
  (let* ((nd  (chunk-level-parent elem))
	 (psl (node-list-filter-by-gi (children (parent nd)) 
				      (chunk-element-list)))
	 (nextlist (car (cdr (split-node-list nd psl)))))
    (if (node-list-empty? nextlist)
	(if (node-list-empty? (parent nd))
	    (empty-node-list)
	    (next-chunk-skip-children (parent nd)))
	(node-list-first nextlist))))

(define (next-chunk-with-children #!optional (elem (current-node)))
  (let* ((nd  (chunk-level-parent elem))
	 (clc (chunk-children nd))
	 (ns  (ifollow-by-gi nd (chunk-element-list))))
    (if (node-list-empty? clc)
	(if (node-list-empty? ns)
	    (next-chunk-skip-children (parent nd))
	    (node-list-first ns))
	;; If the first of the chunk-children (clc) of this element
	;; isn't its own chunk, skip over it, otherwise it's next.
	(if (chunk? (node-list-first clc))
	    (node-list-first clc)
	    (next-chunk-with-children (node-list-first clc))))))
;;	    (if (> (node-list-length clc) 1)
;;		(node-list-first (node-list-rest clc))
;;		(next-chunk-skip-children nd))))))

(define (abs-prev-chunk #!optional (elem (current-node)))
  (let* ((nd  (chunk-parent elem))
	 (pse (ipreced-by-gi nd (chunk-element-list)))
	 (ps  (chunk-parent pse)))
    (if (node-list-empty? ps)
	(parent nd)
	(last-chunk-element ps))))

(define (prev-chunk-element #!optional (elem (current-node)))
  (let* ((nd   (chunk-parent elem))
	 (prev (chunk-parent (abs-prev-chunk nd))))
    ;; There's a special case here.  abs-prev-chunk always returns the last
    ;; chunk element of the preceding element if we walk up the tree.  This
    ;; assures that the last section of the preceding chapter is the "prev"
    ;; element of the current chapter.
    ;;
    ;; However, if chunk-skip-first-element is in use, then abs-prev-chunk
    ;; gets fooled when it tries to find the element that precedes the
    ;; second child element that's in chunk-skip-first-element list.
    ;;
    ;; For example, if SECT1 is in chunk-skip-first-element then the
    ;; chunk that precedes the second SECT1 in a CHAPTER is the CHAPTER
    ;; (not the first SECT1 because the first SECT1 is "skipped", 
    ;; it's in the CHAPTER chunk).  Confused yet?
    ;;
    ;; Ok, now unfortunately, what abs-prev-chunk returns is the last child
    ;; of the CHAPTER, so instead of going from the second SECT1 to the
    ;; CHAPTER, we go from the second SECT1 to the last SECT1 of the CHAPTER.
    ;;
    ;; I can't think of a good way to handle this except to test for it
    ;; right up front.  I wonder if all this skip stuff was really worth it?
    ;;
    (if (and (member (gi elem) (chunk-skip-first-element-list))
	     (equal? (child-number elem) 2))
	;; this is the second child, the prev node is the parent.
	(parent elem)
	;; otherwise, do the "normal" thing to find it:
	(if (node-list-empty? prev)
	    prev
	    (if (combined-chunk? prev)
		(parent prev)
		(if (and (chunk? nd)
			 (chunk? prev)
			 (navigate-to? prev))
		    prev
		    (prev-chunk-element prev)))))))

(define (abs-prev-peer-chunk-element #!optional (elem (current-node)))
  ;; Returns the previous element that is a sibling or parent of the
  ;; current element.  Absolute in this case refers to the fact that
  ;; it returns the immediate predecessor without regard for whether or
  ;; not it is a chunk.
  (let* ((psibling (if (node-list-empty? (preced elem))
		       (empty-node-list)
		       (node-list-last (preced elem)))))
    (if (node-list-empty? psibling)
	(parent elem)
	psibling)))

(define (prev-peer-chunk-element #!optional (elem (current-node)))
  (let loop ((nd (chunk-level-parent elem)))
    (if (node-list-empty? nd)
	(empty-node-list)
	(if (and (chunk? (abs-prev-peer-chunk-element nd))
		 (navigate-to? (abs-prev-peer-chunk-element nd)))
	    (abs-prev-peer-chunk-element nd)
	    (loop (abs-prev-peer-chunk-element nd))))))

(define (prev-major-component-chunk-element #!optional (elem (current-node)) (in-chain #f))
  ;; Return the prev major component of the document that is a sibling (or
  ;; ancestor) of the starting element. This is essentially 'prev-sibling' 
  ;; but skips over things that aren't chunks.
  (if (or (navigate-to? elem) in-chain)
      (if (member (gi elem) (major-component-element-list))
	  (if (node-list-empty? (node-list-last-element (preced elem)))
	      (prev-chunk-element elem)
	      (let ((nd (node-list-last-element (preced elem))))
		(if (navigate-to? nd)
		    nd
		    (prev-major-component-chunk-element nd #t))))
	  (ancestor-member elem (major-component-element-list)))
      (empty-node-list)))

(define (abs-next-chunk #!optional (elem (current-node)) (children-ok? #t))
  (let* ((nd  (chunk-level-parent elem))
	 (clc (if children-ok? (chunk-children nd) (empty-node-list)))
	 (ns  (ifollow-by-gi nd (chunk-element-list))))
    (if (node-list-empty? clc)
	(if (node-list-empty? ns)
	    (if (node-list-empty? (parent nd))
		(empty-node-list)
		(abs-next-chunk (parent nd) #f))
	    (node-list-first ns))
	(node-list-first clc))))

(define (next-chunk-element #!optional (elem (current-node)))
  (let ((next (abs-next-chunk elem)))
    (if (node-list-empty? next)
	(empty-node-list)
	(if (chunk? next)
	    (if (navigate-to? next)
		next
		(next-chunk-element next))
	    (next-chunk-element next)))))

(define (abs-next-peer-chunk-element #!optional (elem (current-node)))
  (let* ((fsibling (if (node-list-empty? (follow elem))
		       (empty-node-list)
		       (node-list-first (follow elem)))))
    (if (node-list-empty? fsibling)
	(if (node-list-empty? (parent elem))
	    (empty-node-list)
	    (abs-next-peer-chunk-element (parent elem)))
	fsibling)))

(define (next-peer-chunk-element #!optional (elem (current-node)))
  (let loop ((nd (chunk-level-parent elem)))
    (if (node-list-empty? nd)
	(empty-node-list)
	(if (and (chunk? (abs-next-peer-chunk-element nd))
		 (navigate-to? (abs-next-peer-chunk-element nd)))
	    (abs-next-peer-chunk-element nd)
	    (loop (abs-next-peer-chunk-element nd))))))

(define (next-major-component-chunk-element #!optional (elem (current-node)) (in-chain #f))
  ;; Return the next major component of the document that is not a descendant
  ;; of the starting element.  This is essentially 'next-sibling' but skips
  ;; over things that aren't chunks.
  (if (or (navigate-to? elem) in-chain)
      (if (member (gi elem) (major-component-element-list))
	  (if (node-list-empty? (node-list-first-element (follow elem)))
	      (next-major-component-chunk-element (parent elem))
	      (let ((nd (node-list-first-element (follow elem))))
		(if (navigate-to? nd)
		    nd
		    (next-major-component-chunk-element nd #t))))
	  (ancestor-member elem (major-component-element-list)))
      (empty-node-list)))

;; ----------------------------------------------------------------------

(define (header-navigation nd)
  (let ((prev  (prev-chunk-element nd))
	(next  (next-chunk-element nd))
	(prevm (prev-major-component-chunk-element nd))
	(nextm (next-major-component-chunk-element nd)))
    (make sequence
      ($html-body-start$)
      (cond 
       ((equal? (gi nd) (normalize "set"))
	(set-header-navigation nd))
       ((equal? (gi nd) (normalize "book"))
	(book-header-navigation nd))
       ((equal? (gi nd) (normalize "part"))
	(part-header-navigation nd))
       ((equal? (gi nd) (normalize "preface"))
	(preface-header-navigation nd))
       ((equal? (gi nd) (normalize "chapter"))
	(chapter-header-navigation nd))
       ((equal? (gi nd) (normalize "article"))
	(article-header-navigation nd))
       ((equal? (gi nd) (normalize "appendix"))
	(appendix-header-navigation nd))
       ((equal? (gi nd) (normalize "reference"))
	(reference-header-navigation nd))
       ((equal? (gi nd) (normalize "refentry"))
	(refentry-header-navigation nd))
       ((equal? (gi nd) (normalize "glossary"))
	(glossary-header-navigation nd))
       ((equal? (gi nd) (normalize "bibliography"))
	(bibliography-header-navigation nd))
       ((equal? (gi nd) (normalize "index"))
	(index-header-navigation nd))
       ;; LegalNotice only happens when %generate-legalnotice-link% is #t
       ((equal? (gi nd) (normalize "legalnotice"))
	(default-header-navigation nd
	  (empty-node-list) (empty-node-list)
	  (empty-node-list) (empty-node-list)))
       ((member (gi nd) (section-element-list)) (section-header-navigation nd))
       (else (default-header-navigation nd prev next prevm nextm)))
      ($user-header-navigation$ prev next prevm nextm)
      ($html-body-content-start$))))

(define (footer-navigation nd)
  (let ((prev  (prev-chunk-element nd))
	(next  (next-chunk-element nd))
	(prevm (prev-major-component-chunk-element nd))
	(nextm (next-major-component-chunk-element nd)))
    (make sequence
      (make-endnotes)
      ($html-body-content-end$)
      ($user-footer-navigation$ prev next prevm nextm)
      (cond 
       ((equal? (gi nd) (normalize "set"))          
	(set-footer-navigation nd))
       ((equal? (gi nd) (normalize "book"))
	(book-footer-navigation nd))
       ((equal? (gi nd) (normalize "part"))
	(part-footer-navigation nd))
       ((equal? (gi nd) (normalize "preface"))
	(preface-footer-navigation nd))
       ((equal? (gi nd) (normalize "chapter"))
	(chapter-footer-navigation nd))
       ((equal? (gi nd) (normalize "article"))
	(article-footer-navigation nd))
       ((equal? (gi nd) (normalize "appendix"))
	(appendix-footer-navigation nd))
       ((equal? (gi nd) (normalize "reference"))
	(reference-footer-navigation nd))
       ((equal? (gi nd) (normalize "refentry"))
	(refentry-footer-navigation nd))
       ((equal? (gi nd) (normalize "glossary"))
	(glossary-footer-navigation nd))
       ((equal? (gi nd) (normalize "bibliography"))
	(bibliography-footer-navigation nd))
       ((equal? (gi nd) (normalize "index"))
        (index-footer-navigation nd))
       ;; LegalNotice only happens when %generate-legalnotice-link% is #t
       ((equal? (gi nd) (normalize "legalnotice"))  
	(default-footer-navigation nd
	  (empty-node-list) (empty-node-list)
	  (empty-node-list) (empty-node-list)))
       ((member (gi nd) (section-element-list)) (section-footer-navigation nd))
       (else (default-footer-navigation nd prev next prevm nextm)))
      (nav-footer nd)
      ($html-body-end$))))

(define (set-header-navigation elemnode)
  (empty-sosofo))
;;  (let ((prev (prev-chunk-element elemnode))
;;	(next (next-chunk-element elemnode))
;;	(prevsib (prev-major-component-chunk-element elemnode))
;;	(nextsib (next-major-component-chunk-element elemnode)))
;;    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (book-header-navigation elemnode)
  (empty-sosofo))
;;  (let ((prev (prev-chunk-element elemnode))
;;	(next (next-chunk-element elemnode))
;;	(prevsib (prev-major-component-chunk-element elemnode))
;;	(nextsib (next-major-component-chunk-element elemnode)))
;;    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (part-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (preface-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (chapter-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (appendix-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (article-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (if (node-list=? elemnode (sgml-root-element))
	(empty-sosofo)
	(default-header-navigation elemnode prev next prevsib nextsib))))

(define (glossary-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (bibliography-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (index-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (reference-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (refentry-header-navigation elemnode)
  (let* ((prev (prev-chunk-element elemnode))
	 (next (next-chunk-element elemnode))
	 (prevsib (prev-major-component-chunk-element elemnode))
	 (nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (section-header-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (set-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (book-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (part-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (preface-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (chapter-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (appendix-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (article-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (glossary-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (bibliography-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (index-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (reference-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (refentry-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (section-footer-navigation elemnode)
  (let ((prev (prev-chunk-element elemnode))
	(next (next-chunk-element elemnode))
	(prevsib (prev-major-component-chunk-element elemnode))
	(nextsib (next-major-component-chunk-element elemnode)))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

;; ----------------------------------------------------------------------

(define (default-header-nav-tbl-ff elemnode prev next prevsib nextsib)
  (let* ((r1? (nav-banner? elemnode))
	 (r1-sosofo (make element gi: "TR"
			  (make element gi: "TH"
				attributes: (list
					     (list "COLSPAN" "5")
					     (list "ALIGN" "center")
					     (list "VALIGN" "bottom"))
				(nav-banner elemnode))))
	 (r2? (or (not (node-list-empty? prev))
		  (not (node-list-empty? next))
		  (not (node-list-empty? prevsib))
		  (not (node-list-empty? nextsib))
		  (nav-context? elemnode)))
	 (r2-sosofo (make element gi: "TR"
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "left")
					     (list "VALIGN" "top"))
				(if (node-list-empty? prev)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to 
							      prev)))
					  (gentext-nav-prev prev))))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "left")
					     (list "VALIGN" "top"))
				(if (node-list-empty? prevsib)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to 
							      prevsib)))
					  (gentext-nav-prevsib prevsib))))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "60%")
					     (list "ALIGN" "center")
					     (list "VALIGN" "bottom"))
				(nav-context elemnode))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "right")
					     (list "VALIGN" "top"))
				(if (node-list-empty? nextsib)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to
							      nextsib)))
					  (gentext-nav-nextsib nextsib))))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "right")
					     (list "VALIGN" "top"))
				(if (node-list-empty? next)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to
							      next)))
					  (gentext-nav-next next)))))))
    (if (or r1? r2?)
	(make element gi: "DIV"
	      attributes: '(("CLASS" "NAVHEADER"))
	  (make element gi: "TABLE"
		attributes: (list
			     (list "WIDTH" %gentext-nav-tblwidth%)
			     (list "BORDER" "0")
			     (list "CELLPADDING" "0")
			     (list "CELLSPACING" "0"))
		(if r1? r1-sosofo (empty-sosofo))
		(if r2? r2-sosofo (empty-sosofo)))
	  (make empty-element gi: "HR"
		attributes: (list
			     (list "ALIGN" "LEFT")
			     (list "WIDTH" %gentext-nav-tblwidth%))))
	(empty-sosofo))))

(define (default-header-nav-tbl-noff elemnode prev next prevsib nextsib)
  (let* ((r1? (nav-banner? elemnode))
	 (r1-sosofo (make element gi: "TR"
			  (make element gi: "TH"
				attributes: (list
					     (list "COLSPAN" "3")
					     (list "ALIGN" "center"))
				(nav-banner elemnode))))
	 (r2? (or (not (node-list-empty? prev))
		  (not (node-list-empty? next))
		  (nav-context? elemnode)))
	 (r2-sosofo (make element gi: "TR"
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "left")
					     (list "VALIGN" "bottom"))
				(if (node-list-empty? prev)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to 
							      prev)))
					  (gentext-nav-prev prev))))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "80%")
					     (list "ALIGN" "center")
					     (list "VALIGN" "bottom"))
				(nav-context elemnode))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "right")
					     (list "VALIGN" "bottom"))
				(if (node-list-empty? next)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to
							      next)))
					  (gentext-nav-next next)))))))
    (if (or r1? r2?)
	(make element gi: "DIV"
	      attributes: '(("CLASS" "NAVHEADER"))
	  (make element gi: "TABLE"
		attributes: (list
			     (list "WIDTH" %gentext-nav-tblwidth%)
			     (list "BORDER" "0")
			     (list "CELLPADDING" "0")
			     (list "CELLSPACING" "0"))
		(if r1? r1-sosofo (empty-sosofo))
		(if r2? r2-sosofo (empty-sosofo)))
	  (make empty-element gi: "HR"
		attributes: (list
			     (list "ALIGN" "LEFT")
			     (list "WIDTH" %gentext-nav-tblwidth%))))
	(empty-sosofo))))

(define (default-header-nav-notbl-ff elemnode prev next prevsib nextsib)
  (make element gi: "DIV"
	attributes: '(("CLASS" "NAVHEADER"))
	(if (nav-banner? elemnode)
	    (make element gi: "H1"
		  (nav-banner elemnode))
	    (empty-sosofo))

	(if (and (node-list-empty? prev)
		 (node-list-empty? prevsib)
		 (node-list-empty? nextsib)
		 (node-list-empty? next))
	    (empty-sosofo)
	    (make element gi: "P"
		  (if (node-list-empty? next)
		      (empty-sosofo)
		      (make sequence 
			(make element gi: "A"
			      attributes: (list
					   (list "HREF" (href-to next)))
			      (gentext-nav-next next))))

		  (if (node-list-empty? prev)
		      (empty-sosofo)
		      (make sequence
			(if (node-list-empty? next)
			    (empty-sosofo)
			    (literal ", "))
			(make element gi: "A"
			      attributes: (list
					   (list "HREF" (href-to prev)))
			      (gentext-nav-prev prev))))
		  
		  (if (node-list-empty? nextsib)
		      (empty-sosofo)
		      (make sequence 
			(if (and (node-list-empty? next)
				 (node-list-empty? prev))
			    (empty-sosofo)
			    (literal ", "))
			(make element gi: "A"
			      attributes: (list
					   (list "HREF" (href-to nextsib)))
			      (gentext-nav-nextsib nextsib))))

		  (if (node-list-empty? prevsib)
		      (empty-sosofo)
		      (make sequence 
			(if (and (node-list-empty? next)
				 (node-list-empty? prev)
				 (node-list-empty? nextsib))
			    (empty-sosofo)
			    (literal ", "))
			(make element gi: "A"
			      attributes: (list
					   (list "HREF" (href-to prevsib)))
			      (gentext-nav-prevsib prevsib))))))
	
	(if (nav-context? elemnode)
	    (make element gi: "H2"
		  (nav-context elemnode))
	    (empty-sosofo))
	
	(make empty-element gi: "HR")))

(define (default-header-nav-notbl-noff elemnode prev next prevsib nextsib)
  (default-header-nav-notbl-ff elemnode prev next 
    (empty-node-list) (empty-node-list)))

(define (default-header-navigation elemnode prev next prevsib nextsib)
  (if %gentext-nav-use-tables%
      (if %gentext-nav-use-ff% 
	  (default-header-nav-tbl-ff elemnode prev next prevsib nextsib)
	  (default-header-nav-tbl-noff elemnode prev next prevsib nextsib))
      (if %gentext-nav-use-ff% 
	  (default-header-nav-notbl-ff elemnode prev next prevsib nextsib)
	  (default-header-nav-notbl-noff elemnode prev next prevsib nextsib))))

(define (default-footer-navigation elemnode prev next prevsib nextsib)
  (if %gentext-nav-use-tables%
      (default-footer-nav-tbl elemnode prev next prevsib nextsib)
      (default-footer-nav-notbl elemnode prev next prevsib nextsib)))

(define (default-footer-nav-tbl elemnode prev next prevsib nextsib)
  (let ((r1? (or (not (node-list-empty? prev))
		 (not (node-list-empty? next))
		 (nav-home? elemnode)))
	(r2? (or (not (node-list-empty? prev))
		 (not (node-list-empty? next))
		 (nav-up? elemnode)))

	(r1-sosofo (make element gi: "TR"
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "33%")
					    (list "ALIGN" "left")
					    (list "VALIGN" "top"))
			       (if (node-list-empty? prev)
				   (make entity-ref name: "nbsp")
				   (make element gi: "A"
					 attributes: (list
						      (list "HREF" (href-to
								    prev)))
					 (gentext-nav-prev prev))))
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "34%")
					    (list "ALIGN" "center")
					    (list "VALIGN" "top"))
			       (nav-home-link elemnode))
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "33%")
					    (list "ALIGN" "right")
					    (list "VALIGN" "top"))
			       (if (node-list-empty? next)
				   (make entity-ref name: "nbsp")
				   (make element gi: "A"
					 attributes: (list
						      (list "HREF" (href-to
								    next)))
					 (gentext-nav-next next))))))
	(r2-sosofo (make element gi: "TR"
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "33%")
					    (list "ALIGN" "left")
					    (list "VALIGN" "top"))
			       (if (node-list-empty? prev)
				   (make entity-ref name: "nbsp")
				   (element-title-sosofo prev)))
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "34%")
					    (list "ALIGN" "center")
					    (list "VALIGN" "top"))
			       (if (nav-up? elemnode)
				   (nav-up elemnode)
				   (make entity-ref name: "nbsp")))
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "33%")
					    (list "ALIGN" "right")
					    (list "VALIGN" "top"))
			       (if (node-list-empty? next)
				   (make entity-ref name: "nbsp")
				   (element-title-sosofo next))))))
    (if (or r1? r2?)
	(make element gi: "DIV"
	      attributes: '(("CLASS" "NAVFOOTER"))
	  (make empty-element gi: "HR"
		attributes: (list
			     (list "ALIGN" "LEFT") 
			     (list "WIDTH" %gentext-nav-tblwidth%)))
	  (make element gi: "TABLE"
		attributes: (list
			     (list "WIDTH" %gentext-nav-tblwidth%)
			     (list "BORDER" "0")
			     (list "CELLPADDING" "0")
			     (list "CELLSPACING" "0"))
		(if r1? r1-sosofo (empty-sosofo))
		(if r2? r2-sosofo (empty-sosofo))))
	(empty-sosofo))))

(define (default-footer-nav-notbl elemnode prev next prevsib nextsib)
  (make element gi: "DIV"
	attributes: '(("CLASS" "NAVFOOTER"))
	(make empty-element gi: "HR")
    
	(if (nav-home? elemnode)
	    (nav-home-link elemnode)
	    (empty-sosofo))

	(if (nav-up? elemnode)
	    (make sequence
	      (if (nav-home? elemnode)
		  (literal ", ")
		  (empty-sosofo))
	      (nav-up elemnode))
	    (empty-sosofo))
    
	(if (or (nav-home? elemnode) (nav-up? elemnode))
	    (make empty-element gi: "BR")
	    (empty-sosofo))

	(if (node-list-empty? prev)
	    (empty-sosofo)
	    (make sequence
	      (make element gi: "A"
		    attributes: (list
				 (list "HREF" (href-to prev)))
		    (gentext-nav-prev prev))
	      (literal ": " (element-title-string prev))
	      (make empty-element gi: "BR")))
	
	(if (node-list-empty? next)
	    (empty-sosofo)
	    (make sequence
	      (make element gi: "A"
		    attributes: (list
				 (list "HREF" (href-to next)))
		    (gentext-nav-next next))
	      (literal ": " (element-title-string next))
	      (make empty-element gi: "BR")))))

