;; $Id: dbverb.dsl,v 1.1 1999/08/20 09:13:56 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define ($format-linenumber$ linenumber inpre?)
  ;; This code is made complex by the fact that we need an additional
  ;; wrapper and we have to translate spaces into nbsp entity-refs,
  ;; if we aren't in a PRE.
  ;; 
  (if (equal? (remainder linenumber %linenumber-mod%) 0)
      (if inpre?
	  (make sequence
	    (literal (pad-string (format-number linenumber "1") 
				 %linenumber-length% %linenumber-padchar%))
	    ($linenumber-space$))
	  (make element gi: "CODE"
		($sp-to-nbsp-sosofo$ 
		 (pad-string (format-number linenumber "1") 
			     %linenumber-length% %linenumber-padchar%))
		($linenumber-space$)))
      (if inpre?
	  (make sequence
	    (literal (pad-string "" %linenumber-length% " "))
	    ($linenumber-space$))
	  (make element gi: "CODE"
		($sp-to-nbsp-sosofo$ 
		 (pad-string "" %linenumber-length% " "))
		($linenumber-space$)))))

(define ($sp-to-nbsp-sosofo$ string)
  ;; Given a string, return it as a sosofo, but replace spaces with 
  ;; nbsp entity-refs.
  (make sequence
    (let loop ((charlist (string->list string))
	       (res (empty-sosofo)))
      (if (null? charlist)
	  res
	  (loop
	   (cdr charlist)
	   (let ((c (car charlist)))
	     (if (equal? c #\ )
		 (sosofo-append res
				(make entity-ref name: "nbsp"))
		 (sosofo-append res (literal (list->string (list c)))))))))))

(define ($verbatim-content$)
  (process-children))

(define ($verbatim-content-with-linenumbers$)
  (make sequence
    ($format-linenumber$ 1 #t)
    (let loop ((kl (children (current-node)))
	       (linecount 1)
	       (res (empty-sosofo)))
      (if (node-list-empty? kl)
	  res
	  (loop
	   (node-list-rest kl)
	   (if (char=? (node-property 'char (node-list-first kl)
				      default: #\U-0000) #\U-000D)
	       (+ linecount 1)
	       linecount)
	   (let ((c (node-list-first kl)))
	     (if (char=? (node-property 'char c default: #\U-0000) 
			 #\U-000D)
		 (sosofo-append res
				(process-node-list c)
				($format-linenumber$ (+ linecount 1) #t))
		 (sosofo-append res (process-node-list c)))))))))

(define ($verbatim-display$ line-numbers?)
  (let ((content (make element gi: "PRE"
		       attributes: (list
				    (list "CLASS" (gi)))
		       (if line-numbers?
			   ($verbatim-content-with-linenumbers$)
			   ($verbatim-content$)))))
    (if %shade-verbatim%
	(make element gi: "TABLE"
	      attributes: ($shade-verbatim-attr$)
	      (make element gi: "TR"
		    (make element gi: "TD"
			  content)))
	content)))

(define ($linespecific-content$)
  ;; Stolen from a dssslist posting by Henry S. Thompson
  (let loop ((kl (children (current-node)))
	     (res (empty-sosofo)))
    (if (node-list-empty? kl)
	res
	(loop
	 (node-list-rest kl)
	 (let ((c (node-list-first kl)))
	   (if (char=? (node-property 'char c default: #\U-0000) 
		       #\U-000D)
	       (sosofo-append res
			      (make empty-element gi: "br")
			      (process-node-list c))
	       (if (char=? (node-property 'char c default: #\U-0000) 
			   #\U-0020)
		   (sosofo-append res
				  (make entity-ref name: "nbsp"))
		   (sosofo-append res (process-node-list c)))))))))

(define ($linespecific-content-with-linenumbers$)
  (make sequence
    ($format-linenumber$ 1 #f)
    (let loop ((kl (children (current-node)))
	       (linecount 1)
	       (res (empty-sosofo)))
      (if (node-list-empty? kl)
	  res
	  (loop
	   (node-list-rest kl)
	   (if (char=? (node-property 'char (node-list-first kl)
				      default: #\U-0000) #\U-000D)
	       (+ linecount 1)
	       linecount)
	   (let ((c (node-list-first kl)))
	     (if (char=? (node-property 'char c default: #\U-0000) 
			 #\U-000D)
		 (sosofo-append res
				(make empty-element gi: "br")
				(process-node-list c)
				($format-linenumber$ (+ linecount 1) #f))
		 (if (char=? (node-property 'char c default: #\U-0000) 
			     #\U-0020)
		     (sosofo-append res
				    (make entity-ref name: "nbsp"))
		     (sosofo-append res (process-node-list c))))))))))
  
(define ($linespecific-display$ line-numbers?)
  (make element gi: "P"
	attributes: '(("CLASS" "LITERALLAYOUT"))
	(if line-numbers?
	    ($linespecific-content-with-linenumbers$)
	    ($linespecific-content$))))

(element literallayout ($linespecific-display$ %number-literallayout-lines%))
(element address ($linespecific-display$ %number-address-lines%))
(element programlisting ($verbatim-display$ %number-programlisting-lines%))
(element screen ($verbatim-display$ %number-screen-lines%))

;; screenshot is a graphic with possible screeninfo
;; *** TO DO: deal with this
(element screenshot (process-children))
(element screeninfo (empty-sosofo))

