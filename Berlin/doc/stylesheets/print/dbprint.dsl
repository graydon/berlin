;; $Id: dbprint.dsl,v 1.1 1999/08/20 09:13:57 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define (HSIZE n)
  (let ((m (if (< n 0) 0 n)))
    (* %bf-size%
       (expt %hsize-bump-factor% m))))

;; ====================== COMMON STYLE TEMPLATES =======================

(define ($block-container$)
  (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	start-indent: %body-start-indent%
	(process-children)))

(define (is-first-para #!optional (para (current-node)))
  ;; A paragraph is the first paragraph if it is preceded by a title
  ;; (or bridgehead) and the only elements that intervene between the
  ;; title and the paragraph are *info elements, indexterms, and beginpage.
  ;;
  (let loop ((nd (ipreced para)))
    (if (node-list-empty? nd)
	;; We've run out of nodes. We still might be the first paragraph
	;; preceded by a title if the parent element has an implied
	;; title.
	(if (equal? (element-title-string (parent para)) "")
	    #f  ;; nope
	    #t) ;; yep
	(if (or (equal? (gi nd) (normalize "title"))
		(equal? (gi nd) (normalize "bridgehead")))
	    #t
	    (if (or (not (equal? (node-property 'class-name nd) 'element))
		    (member (gi nd) (list (normalize "setinfo")
					  (normalize "bookinfo")
					  (normalize "sect1info")
					  (normalize "sect2info")
					  (normalize "sect3info")
					  (normalize "sect4info")
					  (normalize "sect5info")
					  (normalize "refsect1info")
					  (normalize "refsect2info")
					  (normalize "refsect3info")
					  (normalize "refsynopsisdivinfo")
					  (normalize "artheader")
					  (normalize "docinfo")
					  (normalize "indexterm")
					  (normalize "beginpage"))))
		(loop (ipreced nd))
		#f)))))

(define ($paragraph$)
  (make paragraph
	first-line-start-indent: (if (is-first-para)
				     %para-indent-firstpara%
				     %para-indent%)
	space-before: %para-sep%
	space-after: %para-sep%
 	quadding: %default-quadding%
	(process-children-trim)))

(define ($para-container$)
  (make paragraph
	space-before: %para-sep%
	space-after: %para-sep%
	start-indent: (if (member (current-node) (outer-parent-list))
			  %body-start-indent%
			  (inherited-start-indent))
	(process-children-trim)))

(define ($indent-para-container$)
  (make paragraph
	space-before: %para-sep%
	space-after: %para-sep%
	start-indent: (+ (inherited-start-indent) (* (ILSTEP) 2))
	quadding: %default-quadding%
	(process-children-trim)))

(define nop-style
  ;; a nop for use:
  (style
      font-size: (inherited-font-size)))

(define default-text-style
  (style
   font-size: %bf-size%
   font-weight: 'medium
   font-posture: 'upright
   font-family-name: %body-font-family%
   line-spacing: (* %bf-size% %line-spacing-factor%)))

(define ($bold-seq$ #!optional (sosofo (process-children)))
  (make sequence
    font-weight: 'bold
    sosofo))

(define ($italic-seq$ #!optional (sosofo (process-children)))
  (make sequence
    font-posture: 'italic
    sosofo))

(define ($bold-italic-seq$ #!optional (sosofo (process-children)))
  (make sequence
    font-weight: 'bold
    font-posture: 'italic
    sosofo))

(define ($mono-seq$ #!optional (sosofo (process-children)))
  (let ((%factor% (if %verbatim-size-factor% 
		      %verbatim-size-factor% 
		      1.0)))
    (make sequence
      font-family-name: %mono-font-family%
      font-size: (* (inherited-font-size) %factor%)
      sosofo)))

(define ($italic-mono-seq$ #!optional (sosofo (process-children)))
  (make sequence
    font-family-name: %mono-font-family%
    font-posture: 'italic
    sosofo))

(define ($bold-mono-seq$ #!optional (sosofo (process-children)))
  (make sequence
    font-family-name: %mono-font-family%
    font-weight: 'bold
    sosofo))

(define ($score-seq$ stype #!optional (sosofo (process-children)))
  (make score
    type: stype
    sosofo))

(define ($charseq$ #!optional (sosofo (process-children)))
  (make sequence
    sosofo))

(define ($guilabel-seq$ #!optional (sosofo (process-children)))
  (make sequence
    font-family-name: %guilabel-font-family%
    sosofo))

;; Stolen from a posting by James on dssslist
(define *small-caps*
  (letrec ((signature (* #o375 256))
	   (make-afii
	    (lambda (n)
	      (glyph-id (string-append "ISO/IEC 10036/RA//Glyphs::"
				       (number->string n)))))
	   (gen
	    (lambda (from count)
	      (if (= count 0)
		  '()
		  (cons (cons (make-afii from)
			      (make-afii (+ from signature)))
			(gen (+ 1 from)
			     (- count 1)))))))
    (glyph-subst-table (gen #o141 26))))

