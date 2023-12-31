;; $Id: dblists.dsl,v 1.1 1999/08/20 09:13:57 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; =============================== LISTS ================================

(define (BULLTREAT bullfcn ilevel override mark)
  (cond
   (override (bullfcn override ilevel))
   (mark (bullfcn mark ilevel))
   (else (bullfcn "bullet" ilevel))))

(define (BULLSTR m lvl)
  (dingbat m))

(define (BULLSHIFT m lvl)
  (let ((md (case-fold-down m)))
    (case md
	  (("bullet") 0.0em)
	  (("box") (if (= lvl 1) 0.0em 0.1em))
	  (("checkbox") (if (= lvl 1) 0.0em 0.1em))
	  (("check") 0.0em)
	  (("checkedbox") 0.0em)
	  (("dash") 0.0em)
	  (("none") 0.0em)
	  (else 0.0em))))

(define (MSIZE m lvl f1 f2)
  (if (= lvl 1)
      (* %bf-size% f1)
      (* %bf-size% f2)))

(define (BULLSIZE m lvl)
  (let ((md (case-fold-down m)))
    (case md
	  (("bullet") (MSIZE m lvl 0.8 0.72))
	  (("box") (MSIZE m lvl 0.9 0.72))
	  (("checkbox") (MSIZE m lvl 0.9 0.72))
	  (("check") (MSIZE m lvl 1.0 1.0))
	  (("checkedbox") (MSIZE m lvl 1.0 1.0))
	  (("dash") (MSIZE m lvl 1.0 1.0))
	  (("none") (MSIZE m lvl 1.0 1.0))
	  (else (MSIZE m lvl 1.0 1.0)))))

(define (OLSTEP) 0.9em)
;;  (case
;;   (modulo (length (hierarchical-number-recursive (normalize "orderedlist"))) 4)
;;	((1) 1.4em)
;;	((2) 1.4em)
;;	((3) 1.4em)
;;	((0) 1.4em)))

(define (ILSTEP) 1.0em)

(define (COSTEP) 1.5pi)

(define ($list$)
  (make display-group
    start-indent: (if (INBLOCK?)
		      (inherited-start-indent)
		      (+ %block-start-indent% (inherited-start-indent)))
    space-before: (if (INLIST?) %para-sep% %block-sep%)
    space-after:  (if (INLIST?) %para-sep% %block-sep%)))
 
(element itemizedlist ($list$))

(define (process-listitem-content)
  (if (absolute-first-sibling?) 
      (make sequence
	(process-node-list (children (current-node))))
      (next-match)))

(element (listitem programlisting) (process-listitem-content))
(element (listitem screen) (process-listitem-content))
(element (listitem synopsis) (process-listitem-content))
(element (listitem funcsynopsis) (process-listitem-content))
(element (listitem literallayout) (process-listitem-content))
(element (listitem address) (process-listitem-content))
(element (listitem para) (process-listitem-content))
(element (listitem formalpara) (process-listitem-content))
(element (listitem simpara) (process-listitem-content))

(define (generic-list-item indent-step line-field)
  (let* ((itemcontent (children (current-node)))
         (first-child (node-list-first itemcontent))
         (spacing (inherited-attribute-string (normalize "spacing"))))
    (make display-group
      start-indent: (+ (inherited-start-indent) indent-step)
      (make paragraph
        use: (cond
              ((equal? (gi first-child) (normalize "programlisting"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "screen"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "synopsis"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "literallayout"))
               linespecific-style)
              ((equal? (gi first-child) (normalize "address"))
               linespecific-style)
              (else
               nop-style))
        space-before: (if (equal? (normalize "compact") spacing)
                          0pt
                          %para-sep%)
        first-line-start-indent: (- indent-step)
        (make sequence
          line-field)
        (process-node-list first-child))
      (process-node-list (node-list-rest itemcontent)))))

(define (X-generic-list-item indent-step line-field)
  (let* ((itemcontent (children (current-node)))
 	 (first-child (node-list-first itemcontent))
 	 (spacing (inherited-attribute-string (normalize "spacing"))))
    (make sequence
      start-indent: (+ (inherited-start-indent) indent-step)
      (make paragraph
	use: (cond
	      ((equal? (gi first-child) (normalize "programlisting"))
	       verbatim-style)
	      ((equal? (gi first-child) (normalize "screen"))
	       verbatim-style)
	      ((equal? (gi first-child) (normalize "synopsis"))
	       verbatim-style)
	      ((equal? (gi first-child) (normalize "literallayout"))
	       linespecific-style)
	      ((equal? (gi first-child) (normalize "address"))
	       linespecific-style)
	      (else
	       nop-style))
	space-before: (if (equal? (normalize "compact") spacing)
			  0pt
			  %para-sep%)
	first-line-start-indent: (- indent-step)
	(make sequence
	  line-field)
	(with-mode empty-mode (process-node-list first-child)))
;	(process-node-list (children first-child)))
      (process-node-list (node-list-rest itemcontent)))))

(element (itemizedlist listitem)
  (let ((ilevel (length (hierarchical-number-recursive (normalize "itemizedlist"))))
	(override (inherited-attribute-string (normalize "override")))
	(mark (inherited-attribute-string (normalize "mark"))))
    (generic-list-item
     (ILSTEP)
     (if (or (and override
		  (equal? (normalize override) (normalize "none")))
	     (and (not override)
		  (equal? (normalize mark) (normalize "none"))))
	 (make line-field
	   font-size: (BULLTREAT BULLSIZE ilevel override mark)
	   position-point-shift: (BULLTREAT BULLSHIFT ilevel override mark)
	   field-width: (ILSTEP)
	   (literal "\no-break-space;"))
	 (make line-field
	   font-size: (BULLTREAT BULLSIZE ilevel override mark)
	   position-point-shift: (BULLTREAT BULLSHIFT ilevel override mark)
	   field-width: (ILSTEP)
	   (literal (BULLTREAT BULLSTR ilevel override mark)))))))

(element orderedlist ($list$))


(element (orderedlist listitem)
  (let* ((listitems (select-elements (children (parent (current-node)))
				     (normalize "listitem")))
	 (itemnumber (orderedlist-listitem-number (current-node)))
	 (listcount (+ (node-list-length listitems) itemnumber))
	 (factor    (cond 
		     ((> listcount 999) 4)
		     ((> listcount 99) 3)
		     ((> listcount 9) 2)
		     (else 2))))
    (generic-list-item
     (* (OLSTEP) factor)
     (make line-field
       field-width: (* (OLSTEP) factor)
       field-align: 'end
       (literal (number-with-numeration 
		 (inherited-attribute-string (normalize "numeration"))
		 itemnumber)
		(gentext-label-title-sep (normalize "orderedlist")))))))

(define (number-with-numeration numeration number)
  (let* ((depth (length (hierarchical-number-recursive (normalize "orderedlist"))))
	 (rawnum (cond
		  ((equal? numeration (normalize "arabic")) 1)
		  ((equal? numeration (normalize "loweralpha")) 2)
		  ((equal? numeration (normalize "lowerroman")) 3)
		  ((equal? numeration (normalize "upperalpha")) 4)
		  ((equal? numeration (normalize "upperroman")) 0)
		  (else (modulo depth 5))))
	 (num (case rawnum
		((1) (format-number number "1"))
		((2) (format-number number "a"))
		((3) (format-number number "i"))
		((4) (format-number number "A"))
		((0) (format-number number "I")))))
    (if (> depth 5) 
	(string-append "(" num ")")
	num)))
  
(element variablelist ($list$))

(element varlistentry ($para-container$))

(element (varlistentry term)
  (let ((termlength
	  (attribute-string (normalize "termlength")
			    (ancestor (normalize "variablelist")))))
    (make paragraph
	  space-before: %para-sep%
	  keep-with-next?: #t
	  end-indent: (if termlength
			  (- %text-width% (measurement-to-length termlength))
			  0pt)
	  (process-children))))

(element (varlistentry listitem)
  (let ((vle-indent 2em)) ; this ought to be in dbparam!
    (generic-list-item 
     vle-indent
     (make line-field
       field-width: vle-indent
       (literal "\no-break-space;")))))

(define (simplelist-table majororder cols members)
  (let* ((termcount (node-list-length members))
	 (rows (quotient (+ termcount (- cols 1)) cols)))
    (make table
      space-before: (if (INLIST?) %para-sep% %block-sep%)
      space-after:  (if (INLIST?) %para-sep% %block-sep%)
      start-indent: (if (INBLOCK?)
			(inherited-start-indent)
			(+ %block-start-indent% (inherited-start-indent)))
      (if %simplelist-column-width%
	  (let colloop ((colnum 1))
	    (if (> colnum cols)
		(empty-sosofo)
		(make sequence
		  (make table-column
		    width: %simplelist-column-width%)
		  (colloop (+ colnum 1)))))
	  (empty-sosofo))
      (let rowloop ((rownum 1))
	(if (> rownum rows)
	    (empty-sosofo)
	    (make sequence
	      (simplelist-row rownum majororder rows cols members)
	      (rowloop (+ rownum 1))))))))

(define (simplelist-row rownum majororder rows cols members)
  (make table-row
    (let colloop ((colnum 1))
      (if (> colnum cols)
	  (empty-sosofo)
	  (make sequence
	    (simplelist-entry rownum colnum majororder rows cols members)
	    (colloop (+ colnum 1)))))))

(define (simplelist-entry rownum colnum majororder rows cols members)
  (let ((membernum (if (equal? majororder 'row)
		       (+ (* (- rownum 1) cols) colnum)
		       (+ (* (- colnum 1) rows) rownum))))
    (let loop ((nl members) (count membernum))
      (if (<= count 1)
	  (make table-cell
	    column-number: colnum
	    n-columns-spanned: 1
	    n-rows-spanned: 1
;; removed to avoid dependency between dblists and dbtable
;;	    cell-before-row-margin: %cals-cell-before-row-margin%
;;	    cell-after-row-margin: %cals-cell-after-row-margin%
;;	    cell-before-column-margin: %cals-cell-before-column-margin%
;;	    cell-after-column-margin: %cals-cell-after-column-margin%
;;	    start-indent: %cals-cell-content-start-indent%
;;	    end-indent: %cals-cell-content-end-indent%
;; is another variable needed to parameterize these settings, or are
;; constants good enough?
	    cell-before-row-margin: 0pt
	    cell-after-row-margin: 0pt
	    cell-before-column-margin: 3pt
	    cell-after-column-margin: 3pt
	    start-indent: 0pt
	    end-indent: 0pt
	    quadding: 'start
	    (if (node-list-empty? nl)
		(literal "\no-break-space;")
		(process-node-list (node-list-first nl))))
	  (loop (node-list-rest nl) (- count 1))))))

(element simplelist
  (let ((type (attribute-string (normalize "type")))
	(cols (if (attribute-string (normalize "columns"))
		  (if (> (string->number (attribute-string (normalize "columns"))) 0)
		      (string->number (attribute-string (normalize "columns")))
		      1)
		  1))
	(members (select-elements (children (current-node)) (normalize "member"))))
    (cond
       ((equal? type (normalize "inline")) 
	(process-children))
       ((equal? type (normalize "vert"))
	(simplelist-table 'column cols members))
       ((equal? type (normalize "horiz"))
	(simplelist-table 'row    cols members)))))

(element member
  (let ((type (inherited-attribute-string (normalize "type"))))
    (if (equal? type (normalize "inline"))
	(make sequence
	  (process-children)
	  (if (not (last-sibling?))
	      (literal ", ")
	      (literal "")))
	(make paragraph
	  quadding: 'start
	  (process-children)))))

(element segmentedlist (process-children))
(element (segmentedlist title) ($lowtitle$ 2))

(element segtitle (empty-sosofo))
(mode seglist-in-seg
  (element segtitle
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (process-children))))

(element seglistitem ($paragraph$))
(element seg 
  (let* ((seg-num (child-number (current-node)))
	 (seglist (parent (parent (current-node))))
	 (segtitle (nth-node (select-elements 
			 (descendants seglist) (normalize "segtitle")) seg-num)))

    ;; Note: segtitle is only going to be the right thing in a well formed
    ;; SegmentedList.  If there are too many Segs or too few SegTitles,
    ;; you'll get something odd...maybe an error

    (with-mode seglist-in-seg
      (make paragraph
	(make sequence
	  font-family-name: %title-font-family%
	  font-weight: 'bold
	  (sosofo-append (process-node-list segtitle))
	  (literal ": "))
	(process-children)))))

(element calloutlist ($list$))
(element (calloutlist title) ($lowtitle$ 2))

(element callout
  (let* ((calloutcontent (children (current-node)))
	 (arearefs (inherited-attribute-string (normalize "arearefs")))
	 (idlist (split arearefs)))
    (make sequence
      start-indent: (+ (inherited-start-indent) (COSTEP))

      (make paragraph
	space-before: %para-sep%
	first-line-start-indent: (- (COSTEP))
	(make line-field
	  field-width: (COSTEP)
	  (let loop ((ids idlist))
	    (if (null? ids)
		(empty-sosofo)
		(make sequence
		  ($callout-mark$ (element-with-id (car ids)))
		  (loop (cdr ids))))))
	(process-node-list (children (node-list-first calloutcontent))))

      (process-node-list (node-list-rest calloutcontent)))))
