;; $Id: dbbibl.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ......................... BIBLIOGRAPHIC DATA .........................

(define %biblsep% ", ")
(define %biblend% ".")

(element bibliography
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
			    allentries))
	 (title         (element-title-sosofo (current-node)))
	 (body          (make sequence
			  ($component-separator$)
			  ($component-title$)
			  (process-node-list prebibcontent)
			  (process-node-list entries))))
    (html-document title body)))

(element (bibliography title) (empty-sosofo))

(element bibliodiv
  (let* ((allentries (node-list-filter-by-gi (children (current-node))
					     (list (normalize "biblioentry")
						   (normalize "bookbiblio")
						   (normalize "bibliomixed"))))
	 (entries (if biblio-filter-used
		      (biblio-filter allentries)
		      allentries)))
    (if (and biblio-filter-used (node-list-empty? entries))
	(empty-sosofo)
	(make sequence
	  ($section-separator$)
	  ($section-title$)
	  (process-node-list entries)))))

(element (bibliodiv title) (empty-sosofo))

(element biblioentry
  (let* ((proper_children (children (current-node)))
	 ;; improper children? 
	 (other_children (expand-children proper_children
					  (list (normalize "bibliomset")
						(normalize "biblioset"))))
	 (pubdate_children (node-list-filter-by-gi other_children
						   (list (normalize "pubdate"))))

	 ;; Ok, here's the deal.  If PUBDATE is in BIBLIOENTRY, then we
	 ;; use it for the date, otherwise we use the YEAR in COPYRIGHT.
	 ;; So that's what the complex filter below does...
	 (print_children (node-list-filter-by-gi other_children
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
    (make element gi: "P"
	  (make element gi: "A"
		attributes: (list
			     (list "NAME" 
				   (if (attribute-string (normalize "id"))
				       (attribute-string (normalize "id"))
				       (generate-anchor))))
		(empty-sosofo))

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
			  (literal " ")
			  (literal %biblsep%)))
		  (loop (node-list-rest nl))))))))

(element (biblioentry abbrev) 
  (if biblio-number
      (empty-sosofo)
      (make sequence
	(literal "[") (process-children) (literal "] "))))

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
  (make element gi: "I"
	(process-children)))

(element (biblioset title)
  (let ((rel (case-fold-up 
	      (inherited-attribute-string (normalize "relation")))))
  (cond
    ((equal? rel (normalize "article")) (make sequence
					  (literal (gentext-start-quote))
					  (process-children)
					  (literal (gentext-end-quote))))
    (else        (make element gi: "I"
		       (process-children))))))

(element (bibliomset title)
  (let ((rel (case-fold-up 
	      (inherited-attribute-string (normalize "relation")))))
    (cond
     ((equal? rel (normalize "article")) (make sequence
					   (literal (gentext-start-quote))
					   (process-children)
					   (literal (gentext-end-quote))))
     (else        (make element gi: "I"
			(process-children))))))

(element bibliomisc (process-children))

(element bibliomixed
  (let* ((xreflabel (attribute-string (normalize "xreflabel")))
	 (fchild    (node-list-first 
		     (node-list-filter-out-pis (children (current-node)))))
	 (abbrev?   (and (equal? (gi fchild) (normalize "abbrev"))
			 (not biblio-number))))
    (make element gi: "P"
	  (make element gi: "A"
		attributes: (list
			     (list "NAME" 
				   (if (attribute-string (normalize "id"))
				       (attribute-string (normalize "id"))
				       (generate-anchor))))
		(empty-sosofo))

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
  (make element gi: "I"
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
  (let ((years (select-elements (descendants (current-node)) (normalize "year")))
	(holders (select-elements (descendants (current-node)) (normalize "holder"))))
    (make sequence
      (literal (gentext-element-name (gi (current-node))))
      (literal " ")
      (literal (dingbat "copyright"))
      (literal " ")
      (process-node-list years)
      (literal (string-append " " (gentext-by) " "))
      (process-node-list holders))))

(element year
  (make sequence
    (process-children)
    (if (not (last-sibling? (current-node)))
	(literal ", ")
	(empty-sosofo))))

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
(element subjectset (empty-sosofo))
(element volumenum ($charseq$))

(element (bookbiblio revhistory) ($book-revhistory$))

;; The (element (bookinfo revhistory)) construction rule is in dbinfo.dsl
;; It calls $book-revhistory$...
(define ($book-revhistory$)
  (make element gi: "DIV"
	attributes: (list
		     (list "CLASS" (gi)))
	(make element gi: "TABLE"
	      attributes: (list
			   (list "WIDTH" ($table-width$))
			   (list "BORDER" "0"))
	      (make sequence
		(make element gi: "TR"
		      (make element gi: "TH"
			    attributes: '(("ALIGN" "LEFT") 
					  ("VALIGN" "TOP")
					  ("COLSPAN" "3"))
			    (make element gi: "B"
				  (literal (gentext-element-name 
					    (gi (current-node)))))))
	      (process-children)))))

(element (revhistory revision)
  (let ((revnumber (select-elements (descendants (current-node)) (normalize "revnumber")))
	(revdate   (select-elements (descendants (current-node)) (normalize "date")))
	(revauthor (select-elements (descendants (current-node)) (normalize "authorinitials")))
	(revremark (select-elements (descendants (current-node)) (normalize "revremark"))))
    (make sequence
      (make element gi: "TR"
	(make element gi: "TD"
	      attributes: (list
			   (list "ALIGN" "LEFT"))
	      (if (not (node-list-empty? revnumber))
		  (make sequence
		    (literal (gentext-element-name-space 
			      (gi (current-node))))
		    (process-node-list revnumber))
		  (empty-sosofo)))
	(make element gi: "TD"
	      attributes: (list
			   (list "ALIGN" "LEFT"))
	      (if (not (node-list-empty? revdate))
		  (process-node-list revdate)
		  (empty-sosofo)))
	(make element gi: "TD"
	      attributes: (list
			   (list "ALIGN" "LEFT"))
	      (if (not (node-list-empty? revauthor))
		  (make sequence
		    (literal "Revised by: ")
		    (process-node-list revauthor))
		  (empty-sosofo))))
	(make element gi: "TR"
	    (make element gi: "TD"
		  attributes: (list
			       (list "ALIGN" "LEFT")
			       (list "COLSPAN" "3"))
		  (if (not (node-list-empty? revremark))
		      (process-node-list revremark)
		      (empty-sosofo)))))))

(element (revision revnumber) (process-children-trim))
(element (revision date) (process-children-trim))
(element (revision authorinitials) (process-children-trim))
(element (revision revremark) (process-children-trim))
