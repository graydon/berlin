;; $Id: dbbibl.dsl,v 1.1 1999/08/20 09:13:56 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ......................... BIBLIOGRAPHIC DATA .........................

(define %biblsep% ", ")
(define %biblend% ".")

(define (bibliography-content)
  ;; Note that the code below works for both the case where the bibliography
  ;; has BIBLIODIVs and the case where it doesn't, by the slightly subtle
  ;; fact that if it does, then allentries will be (empty-node-list).
  (let* ((allbibcontent (children (current-node)))
	 (prebibcontent (node-list-filter-by-not-gi 
			 allbibcontent
			 (list (normalize "biblioentry")
			       (normalize "bibliomixed"))))
	 (allentries    (node-list-filter-by-gi 
			 allbibcontent
			 (list (normalize "biblioentry")
			       (normalize "bibliomixed"))))
	 (entries       (if biblio-filter-used
			    (biblio-filter allentries)
			    allentries)))
    (make sequence
      (process-node-list prebibcontent)
      (process-node-list entries))))

(element (book bibliography)
  (make simple-page-sequence
    page-n-columns: %page-n-columns%
    page-number-restart?: (or %page-number-restart% 
			      (book-start?) 
			      (first-chapter?))
    page-number-format: ($page-number-format$)
    use: default-text-style
    left-header:   ($left-header$)
    center-header: ($center-header$)
    right-header:  ($right-header$)
    left-footer:   ($left-footer$)
    center-footer: ($center-footer$)
    right-footer:  ($right-footer$)
    start-indent: %body-start-indent%
    input-whitespace-treatment: 'collapse
    quadding: %default-quadding%
    (make sequence
      ($component-title$)
      (bibliography-content))
    (make-endnotes)))

(element bibliography
  ;; A bibliography that's inside something else...
  (let* ((sect  (ancestor-member (current-node) 
				 (append (section-element-list)
					 (component-element-list))))
	 (hlevel 
	  (+ ($section-hlevel$ (gi sect))
	     1))
	 (hs (HSIZE (- 4 hlevel))))
    (make sequence
      (make paragraph
	font-family-name: %title-font-family%
	font-weight:  (if (< hlevel 5) 'bold 'medium)
	font-posture: (if (< hlevel 5) 'upright 'italic)
	font-size: hs
	line-spacing: (* hs %line-spacing-factor%)
	space-before: (* hs %head-before-factor%)
	space-after:  (* hs %head-after-factor%)
	start-indent: (if (or (>= hlevel 3)
			      (member (gi) (list (normalize "refsect1") 
						 (normalize "refsect2") 
						 (normalize "refsect3"))))
			  %body-start-indent%
			  0pt)
	first-line-start-indent: 0pt
	quadding: %section-title-quadding%
	keep-with-next?: #t
	heading-level: (if %generate-heading-level% (+ hlevel 1) 0)
	(element-title-sosofo (current-node)))
      (bibliography-content))))

(element (bibliography title) (empty-sosofo))

(element bibliodiv
  (let* ((allentries (node-list-filter-by-gi (children (current-node))
					     (list (normalize "biblioentry")
						   (normalize "bibliomixed"))))
	 (entries (if biblio-filter-used
		      (biblio-filter allentries)
		      allentries)))
    (if (and biblio-filter-used (node-list-empty? entries))
	(empty-sosofo)
	(make display-group
	  space-before: %block-sep%
	  space-after: %block-sep%
	  start-indent: %body-start-indent%
	  (make sequence
	    ($section-title$)
	    (process-node-list entries))))))

(element (bibliodiv title) (empty-sosofo))

(element biblioentry 
  (let* ((proper_children (children (current-node)))
	 ;; improper children? 
	 (other_children (expand-children proper_children
					  (list (normalize "bibliomset")
						(normalize "biblioset")
						(normalize "bookbiblio"))))
	 (pubdate_children (node-list-filter-by-gi other_children
						   (list (normalize "pubdate"))))

	 ;; Ok, here's the deal.  If PUBDATE is in BIBLIOENTRY, then we
	 ;; use it for the date, otherwise we use the YEAR in COPYRIGHT.
	 ;; So that's what the complex filter below does...
	 (print_children 
	  (node-list-filter-by-gi other_children
				  (append
				   (list (normalize "abbrev")
					 (normalize "authorgroup")
					 (normalize "author")
					 (normalize "title")
					 (normalize "publisher")
					 (normalize "pubdate"))
				   (if (node-list-empty?
					pubdate_children)
				       (list (normalize "copyright"))
				       '())
				   (list (normalize "isbn")
					 (normalize "issn")
					 (normalize "pagenums")))))
	 (has-abbrev?  (equal? (gi (node-list-first print_children))
			       (normalize "abbrev")))
	 (xreflabel    (if (or has-abbrev? biblio-number)
			   #f
			   (attribute-string (normalize "xreflabel")))))
    (make paragraph
      space-before: %para-sep%
      space-after: %para-sep%
      start-indent: (+ (inherited-start-indent) 2pi)
      first-line-start-indent: -2pi

      (if biblio-number 
	  (make sequence
	    (literal "[" 
		     (number->string (bibentry-number (current-node))) 
		     "] "))
	  (empty-sosofo))

      (if xreflabel
	  (make sequence
	    (literal "[" xreflabel "] "))
	  (empty-sosofo))

      (let loop ((nl print_children))
	(if (node-list-empty? nl)
	    (empty-sosofo)
	    (make sequence
	      (process-node-list (node-list-first nl))
	      (if (node-list-empty? (node-list-rest nl))
		  (literal %biblend%)
		  (if (equal? (gi (node-list-first nl)) (normalize "abbrev"))
		      (literal "\no-break-space;")
		      (literal %biblsep%)))
	      (loop (node-list-rest nl))))))))

(element (biblioentry abbrev) 
  (if biblio-number
      (empty-sosofo)
      (make sequence
	(literal "[") ($charseq$) (literal "] "))))

(element (biblioentry copyright)
  ($bibl-node-list$ (select-elements (children (current-node)) 
				     (normalize "year"))))

(element (biblioset copyright)
  ($bibl-node-list$ (select-elements (children (current-node)) 
				     (normalize "year"))))

(mode bibl-year-mode
  (element year
    (process-children)))

(element (biblioentry isbn) 
  (make sequence
    (literal "ISBN: ")
    (process-children)))

(element (biblioentry issn) 
  (make sequence
    (literal "ISSN: ")
    (process-children)))

(element (biblioset isbn) 
  (make sequence
    (literal "ISBN: ")
    (process-children)))

(element (biblioset issn) 
  (make sequence
    (literal "ISSN: ")
    (process-children)))

(element (biblioset pagenums) 
  (make sequence
    (literal (gentext-bibl-pages) " ")
    (process-children)))

(define ($bibl-node-list$ nodes)
  (let loop ((nl nodes) (count 1))
    (if (node-list-empty? nl)
	(empty-sosofo)
	(if (> count 1)
	    (make sequence
	      (literal ", ")
	      (with-mode bibl-year-mode
		(process-node-list (node-list-first nl)))
	      (loop (node-list-rest nl) (+ count 1)))
	    (make sequence
	      (with-mode bibl-year-mode
		(process-node-list (node-list-first nl)))
	      (loop (node-list-rest nl) (+ count 1)))))))

(element (biblioentry publisher)
  ($bibl-node-list$ (select-elements (children (current-node)) 
				     (normalize "publishername"))))

(element (biblioset publisher)
  ($bibl-node-list$ (select-elements (children (current-node)) 
				     (normalize "publishername"))))

(element (biblioentry title) 
  (make sequence
    font-posture: 'italic
    (process-children)))

(element (biblioset title)
  (let ((rel (case-fold-up 
	      (inherited-attribute-string (normalize "relation")))))
    (cond
     ((equal? rel "ARTICLE") (make sequence
			       (literal (gentext-start-quote))
			       (process-children)
			       (literal (gentext-end-quote))))
     (else (make sequence
	     font-posture: 'italic
	     (process-children))))))

(element (bibliomset title)
  (let ((rel (case-fold-up 
	      (inherited-attribute-string (normalize "relation")))))
    (cond
     ((equal? rel "ARTICLE") (make sequence
			       (literal (gentext-start-quote))
			       (process-children)
			       (literal (gentext-end-quote))))
     (else        (make sequence
		    font-posture: 'italic
		    (process-children))))))

(element bibliomisc (process-children))

(element bibliomixed 
  (let* ((xreflabel (and (not biblio-number)
			 (attribute-string (normalize "xreflabel"))))
	 (fchild    (node-list-first 
		     (node-list-filter-out-pis (children (current-node)))))
	 (abbrev?   (and (equal? (gi fchild) (normalize "abbrev"))
			 (not biblio-number))))
    (make paragraph
      space-before: %para-sep%
      space-after: %para-sep%
      start-indent: (+ (inherited-start-indent) 2pi)
      first-line-start-indent: -2pi

      (if biblio-number 
	  (make sequence
	    (literal "[" 
		     (number->string (bibentry-number (current-node))) 
		     "] "))
	  (empty-sosofo))

      (if (and (not abbrev?) xreflabel)
	  (make sequence
	    (literal "[" xreflabel "] "))
	  (empty-sosofo))

      (process-children))))

(element (bibliomixed title) 
  (make sequence
    font-posture: 'italic
    (process-children)))

(element bibliomset (process-children))
(element biblioset (process-children))
(element bookbiblio (process-children))

(element ackno ($charseq$))
(element street ($charseq$))
(element pob ($charseq$))
(element postcode ($charseq$))
(element city ($charseq$))
(element state ($charseq$))
(element country ($charseq$))
(element phone ($charseq$))
(element fax ($charseq$))
(element otheraddr ($charseq$))
(element affiliation ($charseq$))
(element shortaffil ($charseq$))
(element jobtitle ($charseq$))
(element orgdiv ($charseq$))
(element artpagenums ($charseq$))

(element author
  (make sequence
    (literal (author-list-string))))

(element authorgroup (process-children))

(element collab (process-children))
(element collabname ($charseq$))
(element authorinitials ($charseq$))
(element confgroup (process-children))
(element confdates ($charseq$))
(element conftitle ($charseq$))
(element confnum ($charseq$))
(element confsponsor ($charseq$))
(element contractnum ($charseq$))
(element contractsponsor ($charseq$))

(element copyright
  (make paragraph
    (make sequence
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children-trim))))

(element year
  (make sequence
    (process-children)
    (if (not (last-sibling? (current-node)))
	(literal ", ")
	(literal (string-append " " (gentext-by) " ")))))

(element holder ($charseq$))

(element corpauthor
  (make sequence
    (literal (author-list-string))))

(element corpname ($charseq$))
(element date ($charseq$))
(element edition ($charseq$))
(element editor ($charseq$))
(element isbn ($charseq$))
(element issn ($charseq$))
(element invpartnumber ($charseq$))
(element issuenum ($charseq$))

(element legalnotice ($semiformal-object$))
(element (legalnotice title) (empty-sosofo))

(element modespec (empty-sosofo))

(element orgname ($charseq$))

(element othercredit
  (make sequence
    (literal (author-list-string))))

(element pagenums ($charseq$))
(element contrib ($charseq$))

(element firstname ($charseq$))
(element honorific ($charseq$))
(element lineage ($charseq$))
(element othername ($charseq$))
(element surname ($charseq$))

(element printhistory (empty-sosofo))
(element productname ($charseq$))
(element productnumber ($charseq$))
(element pubdate ($charseq$))
(element publisher (process-children))
(element publishername ($charseq$))
(element pubsnumber ($charseq$))
(element releaseinfo (empty-sosofo))
(element revision ($charseq$))
(element revnumber ($charseq$))
(element revremark ($charseq$))
(element seriesvolnums ($charseq$))
(element volumenum ($charseq$))

;; The (element (bookinfo revhistory)) construction rule is in dbinfo.dsl
;; It calls $book-revhistory$...
(define ($book-revhistory$)
  (make sequence
    (make paragraph
      use: title-style
      font-family-name: %title-font-family%
      font-weight: 'bold
      space-before: (* (HSIZE 3) %head-before-factor%)
      space-after: (* (HSIZE 1) %head-before-factor%)
      (literal (gentext-element-name (current-node))))
    (make table
      before-row-border: #f
      (process-children))))

(element (revhistory revision)
  (let ((revnumber (select-elements (descendants (current-node)) (normalize "revnumber")))
	(revdate   (select-elements (descendants (current-node)) (normalize "date")))
	(revauthor (select-elements (descendants (current-node)) (normalize "authorinitials")))
	(revremark (select-elements (descendants (current-node)) (normalize "revremark"))))
    (make sequence
      (make table-row
	(make table-cell
	  column-number: 1
	  n-columns-spanned: 1
	  n-rows-spanned: 1
	  (if (not (node-list-empty? revnumber))
	      (make paragraph
		(make sequence
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber)))
	      (empty-sosofo)))
	(make table-cell
	  column-number: 2
	  n-columns-spanned: 1
	  n-rows-spanned: 1
	  (if (not (node-list-empty? revdate))
	      (make paragraph
		(process-node-list revdate))
	      (empty-sosofo)))
	(make table-cell
	  column-number: 3
	  n-columns-spanned: 1
	  n-rows-spanned: 1
	  (if (not (node-list-empty? revauthor))
	      (make paragraph
		(make sequence
		  (literal "Revised by: ")
		  (process-node-list revauthor)))
	      (empty-sosofo))))
      (make table-row
	cell-after-row-border: #f
	(make table-cell
	  column-number: 1
	  n-columns-spanned: 3
	  n-rows-spanned: 1
	  (if (not (node-list-empty? revremark))
	      (make paragraph
		space-after: %block-sep%
		(process-node-list revremark))
	      (empty-sosofo)))))))

(element (revision revnumber) (process-children-trim))
(element (revision date) (process-children-trim))
(element (revision authorinitials) (process-children-trim))
(element (revision revremark) (process-children-trim))
