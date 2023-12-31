;; $Id: dbblock.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(element highlights ($block-container$))

(element blockquote
  (make element gi: "BLOCKQUOTE"
	(process-children-trim)))

(element epigraph
  (let* ((attrib       (select-elements (children (current-node))
					(normalize "attribution")))
	 (paras        (node-list-filter-by-not-gi
			(children (current-node))
			(list (normalize "attribution")))))
  (make element gi: "TABLE"
	attributes: '(("BORDER" "0")
		      ("WIDTH" "100%")
		      ("CELLSPACING" "0")
		      ("CELLPADDING" "0")
		      ("CLASS" "EPIGRAPH"))
	(make element gi: "TR"
	      (make element gi: "TD"
		    attributes: '(("WIDTH" "45%"))
		    (make entity-ref name: "nbsp"))
	      (make element gi: "TD"
		    attributes: '(("WIDTH" "45%")
				  ("ALIGN" "LEFT")
				  ("VALIGN" "TOP"))
		    (make element gi: "I"
			  (process-node-list paras))))
	(if (node-list-empty? attrib)
	    (empty-sosofo)
	    (make element gi: "TR"
		  (make element gi: "TD"
			attributes: '(("WIDTH" "45%"))
			(make entity-ref name: "nbsp"))
		  (make element gi: "TD"
			attributes: '(("WIDTH" "45%")
				      ("ALIGN" "RIGHT")
				      ("VALIGN" "TOP"))
			(make element gi: "I"
			      (process-node-list attrib))))))))

(element attribution ($charseq$))

(element (epigraph para)
  (make element gi: "P"
	(make element gi: "I"
	      (process-children-trim))))

(element para ($paragraph$))
(element simpara ($paragraph$))

(element formalpara
  (make element gi: "DIV"
	attributes: (list
		     (list "CLASS" (gi)))
	(make element gi: "P"
	      (process-children))))

(element (formalpara title) ($runinhead$))

(element (formalpara para)
  (process-children))

(element sidebar 
  (make element gi: "TABLE"
	attributes: (list
		     (list "CLASS" (gi))
		     (list "BORDER" "1")
		     (list "CELLPADDING" "5"))
	(make element gi: "TR"
	      (make element gi: "TD"
		    ($semiformal-object$)))))

(element (sidebar title)
  (empty-sosofo))

(element abstract 
  (make element gi: "BLOCKQUOTE"
	attributes: '(("CLASS" "ABSTRACT"))
	($semiformal-object$)))

(element authorblurb ($block-container$))

(define ($inline-object$)
  (process-children))

(define ($informal-object$)
  (let ((id (attribute-string (normalize "id"))))
    (make element gi: "DIV"
	  attributes: (list
		       (list "CLASS" (gi)))
	  (if id
	      (make element gi: "A"
		    attributes: (list (list "NAME" id))
		    (empty-sosofo))
	      (empty-sosofo))

	  (if %spacing-paras%
	      (make element gi: "P" (empty-sosofo))
	      (empty-sosofo))
	  
	  (process-children)
	  
	  (if %spacing-paras%
	      (make element gi: "P" (empty-sosofo))
	      (empty-sosofo)))))
  
(define (object-title-after #!optional (node (current-node))) 
  (if (member (gi node) ($object-titles-after$))
      #t
      #f))

(define ($formal-object$)
  (let* ((nsep  (gentext-label-title-sep (gi)))
	 (id    (attribute-string (normalize "id")))
	 (title-inline-sosofo 
	        (make sequence
		  (literal (gentext-element-name (gi)))
		  (if (string=? (element-label) "")
		      (literal nsep)
		      (literal " " (element-label) nsep))
		  (with-mode title-mode
		    (process-node-list 
		     (select-elements (children (current-node))
				      (normalize "title"))))))
	 (title-sosofo (make element gi: "P"
			     (make element gi: "B"
				   (if id
				       (make element gi: "A"
					     attributes: (list 
							  (list "NAME" id))
					     title-inline-sosofo)
				       title-inline-sosofo))))
	 (object-sosofo (process-children)))
    (if (object-title-after)
	(make element gi: "DIV" 
	      attributes: (list
			   (list "CLASS" (gi)))
	      object-sosofo
	      title-sosofo)
	(make element gi: "DIV" 
	      attributes: (list
			   (list "CLASS" (gi)))
	      title-sosofo
	      object-sosofo))))

(define ($semiformal-object$)
  ;; semiformal means optional title...
  (if (node-list-empty? (select-elements (children (current-node)) 
					 (normalize "title")))
      ($informal-object$)
      ($formal-object$)))

(element example ($formal-object$))

(element (example title) (empty-sosofo)) ; don't show caption below example
(element informalexample ($informal-object$))

(element (figure title) (empty-sosofo)) ; don't show caption below figure

(element figure ($formal-object$))

(element informaltable ($informal-object$))

(element table ($formal-object$))

(element (table title) (empty-sosofo))
