;; $Id: dbinline.dsl,v 1.1 1999/08/20 09:13:57 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================== INLINES ===============================

(element accel ($charseq$))
(element action ($charseq$))
(element application ($charseq$))
(element classname ($mono-seq$))
(element command ($bold-seq$))
(element computeroutput ($mono-seq$))
(element database ($charseq$))
(element email 
  (make sequence (literal "&#60;") ($mono-seq$) (literal "&#62;")))
(element envar ($charseq$))
(element errorcode ($charseq$))
(element errorname ($charseq$))
(element errortype ($charseq$))
(element filename ($mono-seq$))
(element function ($mono-seq$))
(element guibutton ($guilabel-seq$))
(element guiicon ($guilabel-seq$))
(element guilabel ($guilabel-seq$))
(element guimenu ($guilabel-seq$))
(element guimenuitem ($guilabel-seq$))
(element guisubmenu ($guilabel-seq$))
(element hardware ($charseq$))
(element interface ($charseq$))
(element interfacedefinition ($charseq$))
(element keycap ($bold-seq$))
(element keycode ($charseq$))

(element keycombo 
  (let* ((action (attribute-string (normalize "action")))
	 (joinchar 
	  (cond
	   ((equal? action (normalize "seq")) " ")          ;; space
	   ((equal? action (normalize "simul")) "+")        ;; +
	   ((equal? action (normalize "press")) "-")        ;; ? I don't know
	   ((equal? action (normalize "click")) "-")        ;; ? what to do
	   ((equal? action (normalize "double-click")) "-") ;; ? about the rest
	   ((equal? action (normalize "other")) "-")        ;; ? of these
	   (else "-"))))
    (let loop ((nl (children (current-node))) (count 1))
      (if (node-list-empty? nl)
	  (empty-sosofo)
	  (if (equal? count 1)
	      (make sequence
		(process-node-list (node-list-first nl))
		(loop (node-list-rest nl) (+ count 1)))
	      (make sequence
		(literal joinchar)
		(process-node-list (node-list-first nl))
		(loop (node-list-rest nl) (+ count 1))))))))

(element keysym ($charseq$))
(element literal ($mono-seq$))
(element medialabel ($italic-seq$))
(element menuchoice ($charseq$))
(element shortcut ($bold-seq$))
(element mousebutton ($charseq$))
(element option ($charseq$))

(element optional 
  (make sequence 
    (literal %arg-choice-opt-open-str%)
    ($charseq$)
    (literal %arg-choice-opt-close-str%)))

(element parameter ($italic-mono-seq$))
(element property ($charseq$))
(element prompt ($mono-seq$))
(element replaceable ($italic-mono-seq$))
(element returnvalue ($charseq$))
(element structfield ($italic-mono-seq$))
(element structname ($charseq$))
(element symbol ($charseq$))
(element systemitem ($charseq$))
(element token ($charseq$))
(element type ($charseq$))
(element userinput ($bold-mono-seq$))
(element abbrev ($charseq$))
(element acronym ($charseq$))

(element citation 
  (if biblio-citation-check
      (let* ((bgraphies (select-elements (descendants (sgml-root-element))
					 (normalize "bibliography")))
	     (bchildren1 (expand-children bgraphies
					  (list (normalize "bibliography"))))
	     (bchildren2 (expand-children bchildren1
					  (list (normalize "bibliodiv"))))
	     (bibentries (node-list-filter-by-gi 
			  bchildren2
			  (list (normalize "biblioentry")
				(normalize "bibliomixed")))))
	(let loop ((bibs bibentries))
	  (if (node-list-empty? bibs)
	      (make sequence
		(error (string-append "Cannot find citation: " 
					   (data (current-node))))
		(literal "[") ($charseq$) (literal "]"))
	      (if (citation-matches-target? (current-node) 
					    (node-list-first bibs))
		  (make link 
		    destination: (node-list-address (node-list-first bibs))
		    (literal "[") ($charseq$) (literal "]"))
		  (loop (node-list-rest bibs))))))
      (make sequence 
	(literal "[") ($charseq$) (literal "]"))))

(element citerefentry ($charseq$))
(element citetitle ($italic-seq$))
(element emphasis 
	 (case (attribute-string "role" (current-node))
	   (("bold") ($bold-seq$))
	   (("bold-italic") ($bold-italic-seq$))
	   (("underline") ($score-seq$ 'after))
	   (else ($italic-seq$))))

(element firstterm ($italic-seq$))
(element foreignphrase ($italic-seq$))
(element markup ($charseq$))
(element phrase ($charseq$))

(element quote
  (make sequence
    (literal (gentext-start-quote))
    (process-children)
    (literal (gentext-end-quote))))

(element sgmltag
  (let ((class (if (attribute-string (normalize "class"))
		   (attribute-string (normalize "class"))
		   (normalize "element"))))
    (cond
<![CDATA[
      ((equal? class (normalize "attribute")) ($charseq$))
      ((equal? class (normalize "attvalue")) ($mono-seq$))
      ((equal? class (normalize "element")) ($charseq$))
      ((equal? class (normalize "endtag")) ($mono-seq$ (make sequence 
			       (literal "</") 
			       (process-children)
			       (literal ">"))))
      ((equal? class (normalize "genentity")) ($mono-seq$ (make sequence
				    (literal "&")
				    (process-children)
				    (literal ";"))))
      ((equal? class (normalize "numcharref")) ($mono-seq$ (make sequence
				     (literal "&#")
				     (process-children)
				     (literal ";"))))
      ((equal? class (normalize "paramentity")) ($mono-seq$ (make sequence
				      (literal "%")
				      (process-children)
				      (literal ";"))))
      ((equal? class (normalize "pi")) ($mono-seq$ (make sequence 
			    (literal "<?")
			    (process-children)
			    (literal ">"))))
      ((equal? class (normalize "starttag")) ($mono-seq$ (make sequence 
				  (literal "<") 
				  (process-children)
				  (literal ">"))))
      ((equal? class (normalize "sgmlcomment")) ($mono-seq$ (make sequence 
				     (literal "<!--")
				     (process-children)
				     (literal "-->"))))
]]>
      (else ($charseq$)))))

(element trademark ($charseq$))
(element wordasword ($italic-seq$))

(element lineannotation
  (make sequence
    font-family-name: %body-font-family%
    font-posture: 'italic
    (process-children)))

(define ($ss-seq$ plus-or-minus #!optional (sosofo (process-children)))
  (make sequence
	font-size:
	  (* (inherited-font-size) %ss-size-factor%)
	position-point-shift:
	  (plus-or-minus (* (inherited-font-size) %ss-shift-factor%))
	sosofo))

(element superscript ($ss-seq$ +))
(element subscript ($ss-seq$ -))

(element comment (empty-sosofo))

