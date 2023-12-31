;; $Id: dbdivis.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================= DIVISIONS ==============================

(element set
  (let* ((setinfo  (select-elements (children (current-node)) (normalize "setinfo")))
	 (ititle   (select-elements (children setinfo) (normalize "title")))
	 (title    (if (node-list-empty? ititle)
		       (select-elements (children (current-node)) (normalize "title"))
		       (node-list-first ititle)))
	 (nl       (titlepage-info-elements (current-node) setinfo))
	 (tsosofo  (with-mode head-title-mode
		     (process-node-list title))))
    (html-document 
     tsosofo
     (make element gi: "DIV"
	   attributes: '(("CLASS" "SET"))
	   (if %generate-set-titlepage%
	       (make sequence
		 (set-titlepage nl 'recto)
		 (set-titlepage nl 'verso))
	       (empty-sosofo))
	   
	   (if (not (generate-toc-in-front))
	       (process-children)
	       (empty-sosofo))
	   
	   (if %generate-set-toc%
	       (make sequence
		 (build-toc (current-node) (toc-depth (current-node))))
	       (empty-sosofo))
	   
	   (if (generate-toc-in-front)
	       (process-children)
	       (empty-sosofo))))))

(element (set title) (empty-sosofo))

(element book 
  (let* ((bookinfo  (select-elements (children (current-node)) (normalize "bookinfo")))
	 (ititle   (select-elements (children bookinfo) (normalize "title")))
	 (title    (if (node-list-empty? ititle)
		       (select-elements (children (current-node)) (normalize "title"))
		       (node-list-first ititle)))
	 (nl       (titlepage-info-elements (current-node) bookinfo))
	 (tsosofo  (with-mode head-title-mode
		     (process-node-list title)))
	 (dedication (select-elements (children (current-node)) (normalize "dedication"))))
    (html-document 
     tsosofo
     (make element gi: "DIV"
	   attributes: '(("CLASS" "BOOK"))
	   (if %generate-book-titlepage%
	       (make sequence
		 (book-titlepage nl 'recto)
		 (book-titlepage nl 'verso))
	       (empty-sosofo))
	  
	   (if (node-list-empty? dedication)
	       (empty-sosofo)
	       (with-mode dedication-page-mode
		 (process-node-list dedication)))

	   (if (not (generate-toc-in-front))
	       (process-children)
	       (empty-sosofo))
	  
	   (if %generate-book-toc%
	       (build-toc (current-node) (toc-depth (current-node)))
	       (empty-sosofo))
	  
;;	  (let loop ((gilist %generate-book-lot-list%))
;;	    (if (null? gilist)
;;		(empty-sosofo)
;;		(if (not (node-list-empty? 
;;			  (select-elements (descendants (current-node))
;;					   (car gilist))))
;;		    (make sequence
;;		      (build-lot (current-node) (car gilist))
;;		      (loop (cdr gilist)))
;;		    (loop (cdr gilist)))))
	  
	   (if (generate-toc-in-front)
	       (process-children)
	       (empty-sosofo))))))

(element (book title) (empty-sosofo))

(element part
  (let* ((partinfo (select-elements (children (current-node)) (normalize "docinfo")))
	 (partintro (select-elements (children (current-node)) (normalize "partintro")))
	 (nl       (titlepage-info-elements 
		    (current-node) 
		    partinfo
		    (if %generate-partintro-on-titlepage%
			partintro
			(empty-node-list))))
	 (ititle   (select-elements (children partinfo) (normalize "title")))
	 (title    (if (node-list-empty? ititle)
		       (select-elements (children (current-node)) (normalize "title"))
		       (node-list-first ititle)))
	 (tsosofo  (with-mode head-title-mode
		     (process-node-list title))))
    (html-document 
     tsosofo
     (make element gi: "DIV"
	   attributes: '(("CLASS" "PART"))

	   (if %generate-part-titlepage%
	       (make sequence
		 (part-titlepage nl 'recto)
		 (part-titlepage nl 'verso))
	       (empty-sosofo))

	   (if (not (generate-toc-in-front))
	       (process-children)
	       (empty-sosofo))

	   (if (and %generate-part-toc%
		    (not %generate-part-toc-on-titlepage%))
	       (make sequence
		 (build-toc (current-node)
			    (toc-depth (current-node))))
	       (empty-sosofo))

	   (if (and (not (node-list-empty? partintro))
		    (not %generate-partintro-on-titlepage%))
	       ($process-partintro$ partintro)
	       (empty-sosofo))
	   
	   (if (generate-toc-in-front)
	       (process-children)
	       (empty-sosofo))))))
  
(element (part title) (empty-sosofo))

(element partintro (empty-sosofo))

(element (partintro title)
  (make element gi: "H1"
	(process-children)))

(element (partintro sect1)
  ($section-body$))

(define ($process-partintro$ partintro)
  (make element gi: "DIV"
	attributes: (list (list "CLASS" "PARTINTRO"))
	(process-node-list (children partintro))))
