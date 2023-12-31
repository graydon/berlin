;; $Id: dbverb.dsl,v 1.1 1999/08/20 09:13:57 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define verbatim-style
  (style
      font-family-name: %mono-font-family%
      font-size:        (* (inherited-font-size) 
			   (if %verbatim-size-factor%
			       %verbatim-size-factor%
			       1.0))
      font-weight:      'medium
      font-posture:     'upright
      line-spacing:     (* (* (inherited-font-size) 
			      (if %verbatim-size-factor%
				  %verbatim-size-factor%
				  1.0))
			   %line-spacing-factor%)
      first-line-start-indent: 0pt
      lines: 'asis
      input-whitespace-treatment: 'preserve
      quadding: 'start))

(define linespecific-style
  (style
      first-line-start-indent: 0pt
      lines: 'asis
      input-whitespace-treatment: 'preserve
      quadding: 'start))

(define ($format-linenumber$ linenumber)
  ;; A line-field would make more sense here, and allow proportional
  ;; fonts, but you can't put line-fields in the middle of a paragraph
  ;; in the current RTF backend of Jade
  (let ((%factor% (if %verbatim-size-factor%
		      %verbatim-size-factor%
		      1.0)))
    (if (equal? (remainder linenumber %linenumber-mod%) 0)
	(make sequence
	  use: verbatim-style
	  (literal (pad-string (format-number linenumber "1") 
			       %linenumber-length% %linenumber-padchar%))
	  ($linenumber-space$))
	(make sequence
	  use: verbatim-style
	  (literal (pad-string "" %linenumber-length% " "))
	  ($linenumber-space$)))))

(define ($linespecific-content$)
  (process-children))

(define ($linespecific-content-with-linenumbers$)
  (make sequence
    ($format-linenumber$ 1)
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
				($format-linenumber$ (+ linecount 1)))
		 (sosofo-append res (process-node-list c)))))))))

(define ($verbatim-display$ line-numbers?)
  (let* ((width-in-chars (if (attribute-string (normalize "width"))
			     (string->number (attribute-string (normalize "width")))
			     %verbatim-default-width%))
	 (fsize (lambda () (if (or (attribute-string (normalize "width"))
				   (not %verbatim-size-factor%))
			       (/ (/ (- %text-width% (inherited-start-indent))
				     width-in-chars) 
				  0.7)
			       (* (inherited-font-size) 
				  %verbatim-size-factor%))))
	 (vspace (if (INBLOCK?)
		     0pt
		     (if (INLIST?)
			 %para-sep% 
			 %block-sep%))))
    (make paragraph
      use: verbatim-style
      space-before: (if (and (string=? (gi (parent)) (normalize "entry"))
 			     (absolute-first-sibling?))
			0pt
			vspace)
      space-after:  (if (and (string=? (gi (parent)) (normalize "entry"))
 			     (absolute-last-sibling?))
			0pt
			vspace)
      font-size: (fsize)
      line-spacing: (* (fsize) %line-spacing-factor%)
      start-indent: (if (INBLOCK?)
			(inherited-start-indent)
			(+ %block-start-indent% (inherited-start-indent)))
      (if line-numbers?
	  ($linespecific-content-with-linenumbers$)
	  ($linespecific-content$)))))

(define ($linespecific-display$ line-numbers?)
  (let ((vspace (if (INBLOCK?)
		   0pt
		   (if (INLIST?) 
		       %para-sep% 
		       %block-sep%))))
    (make paragraph
      use: linespecific-style
      space-before: (if (and (string=? (gi (parent)) (normalize "entry"))
 			     (absolute-first-sibling?))
			0pt
			vspace)
      space-after:  (if (and (string=? (gi (parent)) (normalize "entry"))
 			     (absolute-last-sibling?))
			0pt
			vspace)
      start-indent: (if (INBLOCK?)
			(inherited-start-indent)
			(+ %block-start-indent% (inherited-start-indent)))
      (if line-numbers?
	  ($linespecific-content-with-linenumbers$)
	  ($linespecific-content$)))))

(element literallayout ($linespecific-display$ %number-literallayout-lines%))
(element address ($linespecific-display$ %number-address-lines%))
(element programlisting ($verbatim-display$ %number-programlisting-lines%))
(element screen ($verbatim-display$ %number-screen-lines%))

(element screenshot (process-children))
(element screeninfo (empty-sosofo))

