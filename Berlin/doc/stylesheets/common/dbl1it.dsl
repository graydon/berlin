;; $Id: dbl1it.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; ----------------------------- Localization -----------------------------

;; If you create a new version of this file, please send it to
;; Norman Walsh, ndw@nwalsh.com

;; The generated text for cross references to elements.  See dblink.dsl
;; for a discussion of how substitution is performed on the %x
;; keywords.
;;

(define (it-appendix-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Appendix; %n"
      "&appendix; %t"))

(define (it-article-xref-string gi-or-name)
  (string-append %gentext-it-start-quote%
		 "%t"
		 %gentext-it-end-quote%))

(define (it-bibliography-xref-string gi-or-name)
  "%t")

(define (it-book-xref-string gi-or-name)
  "%t")

(define (it-chapter-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Chapter; %n"
      "il &chapter; %t"))

(define (it-equation-xref-string gi-or-name)
  "&Equation; %n")

(define (it-example-xref-string gi-or-name)
  "&Example; %n")

(define (it-figure-xref-string gi-or-name)
  "&Figure; %n")

(define (it-listitem-xref-string gi-or-name)
  "%n")

(define (it-part-xref-string gi-or-name)
  "&Part; %n")

(define (it-preface-xref-string gi-or-name)
  "%t")

(define (it-procedure-xref-string gi-or-name)
  "&Procedure; %n, %t")

(define (it-section-xref-string gi-or-name)
  (if %section-autolabel% 
      "la &Section; %n" 
      "la &section; %t"))

(define (it-sect1-xref-string gi-or-name)
  (it-section-xref-string gi-or-name))

(define (it-sect2-xref-string gi-or-name)
  (it-section-xref-string gi-or-name))

(define (it-sect3-xref-string gi-or-name)
  (it-section-xref-string gi-or-name))

(define (it-sect4-xref-string gi-or-name)
  (it-section-xref-string gi-or-name))

(define (it-sect5-xref-string gi-or-name)
  (it-section-xref-string gi-or-name))

(define (it-step-xref-string gi-or-name)
  "&step; %n")

(define (it-table-xref-string gi-or-name)
  "&Table; %n")

(define (it-default-xref-string gi-or-name)
  (let* ((giname (if (string? gi-or-name) gi-or-name (gi gi-or-name)))
	 (msg    (string-append "[&xrefto; "
				(if giname giname "&nonexistantelement;")
				" &unsupported;]"))
	 (err    (node-list-error msg (current-node))))
    msg))

(define (gentext-it-xref-strings gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
      ((equal? name (normalize "appendix")) (it-appendix-xref-string gind))
      ((equal? name (normalize "article"))  (it-article-xref-string gind))
      ((equal? name (normalize "bibliography")) (it-bibliography-xref-string gind))
      ((equal? name (normalize "book"))     (it-book-xref-string gind))
      ((equal? name (normalize "chapter"))  (it-chapter-xref-string gind))
      ((equal? name (normalize "equation")) (it-equation-xref-string gind))
      ((equal? name (normalize "example"))  (it-example-xref-string gind))
      ((equal? name (normalize "figure"))   (it-figure-xref-string gind))
      ((equal? name (normalize "listitem")) (it-listitem-xref-string gind))
      ((equal? name (normalize "part"))     (it-part-xref-string gind))
      ((equal? name (normalize "preface"))  (it-preface-xref-string gind))
      ((equal? name (normalize "procedure")) (it-procedure-xref-string gind))
      ((equal? name (normalize "sect1"))    (it-sect1-xref-string gind))
      ((equal? name (normalize "sect2"))    (it-sect2-xref-string gind))
      ((equal? name (normalize "sect3"))    (it-sect3-xref-string gind))
      ((equal? name (normalize "sect4"))    (it-sect4-xref-string gind))
      ((equal? name (normalize "sect5"))    (it-sect5-xref-string gind))
      ((equal? name (normalize "step"))     (it-step-xref-string gind))
      ((equal? name (normalize "table"))    (it-table-xref-string gind))
      (else (it-default-xref-string gind)))))

(define (it-auto-xref-indirect-connector before) 
  (cond 
   ((member (gi before)
         (list (normalize "part")
               (normalize "set")
               (normalize "revision")
               (normalize "dedication")
               (normalize "bibliography")
               (normalize "preface")
               (normalize "figure")
               (normalize "procedure")
               (normalize "sidebar")
               (normalize "table")
               (normalize "sect1")
               (normalize "sect2")
               (normalize "sect3")
               (normalize "sect4")
               (normalize "sect5")
               (normalize "simplesect")))
    (literal " nella "))
   ((member (gi before)
         (list (normalize "appendix")
               (normalize "index")
               (normalize "abstract")
               (normalize "equation")
               (normalize "example")
               (normalize "article")))
    (literal " nell'"))
   ((member (gi before)
         (list (normalize "book")
               (normalize "chapter")
               (normalize "step")
               (normalize "reference")
               (normalize "glossary")))
    (literal " nel "))
;; This is supposed never to be triggered!
;; I think the above conditions cover all the possible kinds of link
;; target containers (it's probably a superset, indeed). If they
;; don't, add the offending element to the correspondant list. 
   (else
    (literal "[&unexpectedelementname;]"))))

;; Should the TOC come first or last?
;;
(define %generate-it-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define it-abstract-name	"&Abstract;")
(define it-appendix-name	"&Appendix;")
(define it-article-name	"&Article;")
(define it-bibliography-name	"&Bibliography;")
(define it-book-name		"&Book;")
(define it-calloutlist-name	"")
(define it-caution-name	"&Caution;")
(define it-chapter-name	"&Chapter;")
(define it-copyright-name	"&Copyright;")
(define it-dedication-name	"&Dedication;")
(define it-edition-name	"&Edition;")
(define it-equation-name	"&Equation;")
(define it-example-name	"&Example;")
(define it-figure-name	"&Figure;")
(define it-glossary-name	"&Glossary;")
(define it-glosssee-name	"&See;")
(define it-glossseealso-name	"&SeeAlso;")
(define it-important-name	"&Important;")
(define it-index-name		"&Index;")
(define it-setindex-name	"&SetIndex;")
(define it-isbn-name		"&ISBN;")
(define it-legalnotice-name	"")
(define it-msgaud-name	"&Audience;")
(define it-msglevel-name	"&Level;")
(define it-msgorig-name	"&Origin;")
(define it-note-name		"&Note;")
(define it-part-name		"&Part;")
(define it-preface-name	"&Preface;")
(define it-procedure-name	"&Procedure;")
(define it-pubdate-name	"&Published;")
(define it-reference-name	"&Reference;")
(define it-refname-name	"&Name;")
(define it-revhistory-name	"&RevisionHistory;")
(define it-revision-name	"&Revision;")
(define it-sect1-name		"&Section;")
(define it-sect2-name		"&Section;")
(define it-sect3-name		"&Section;")
(define it-sect4-name		"&Section;")
(define it-sect5-name		"&Section;")
(define it-simplesect-name	"&Section;")
(define it-seeie-name		"&See;")
(define it-seealsoie-name	"&Seealso;")
(define it-set-name		"&Set;")
(define it-sidebar-name	"&Sidebar;")
(define it-step-name		"&step;")
(define it-table-name		"&Table;")
(define it-tip-name		"&Tip;")
(define it-toc-name		"&TableofContents;")
(define it-warning-name	"&Warning;")

(define (gentext-it-element-name gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract"))	it-abstract-name)
     ((equal? name (normalize "appendix"))	it-appendix-name)
     ((equal? name (normalize "article"))	it-article-name)
     ((equal? name (normalize "bibliography"))	it-bibliography-name)
     ((equal? name (normalize "book"))		it-book-name)
     ((equal? name (normalize "calloutlist"))	it-calloutlist-name)
     ((equal? name (normalize "caution"))	it-caution-name)
     ((equal? name (normalize "chapter"))	it-chapter-name)
     ((equal? name (normalize "copyright"))	it-copyright-name)
     ((equal? name (normalize "dedication"))	it-dedication-name)
     ((equal? name (normalize "edition"))	it-edition-name)
     ((equal? name (normalize "equation"))	it-equation-name)
     ((equal? name (normalize "example"))	it-example-name)
     ((equal? name (normalize "figure"))	it-figure-name)
     ((equal? name (normalize "glossary"))	it-glossary-name)
     ((equal? name (normalize "glosssee"))	it-glosssee-name)
     ((equal? name (normalize "glossseealso"))	it-glossseealso-name)
     ((equal? name (normalize "important"))	it-important-name)
     ((equal? name (normalize "index"))		it-index-name)
     ((equal? name (normalize "setindex"))	it-setindex-name)
     ((equal? name (normalize "isbn"))		it-isbn-name)
     ((equal? name (normalize "legalnotice"))	it-legalnotice-name)
     ((equal? name (normalize "msgaud"))	it-msgaud-name)
     ((equal? name (normalize "msglevel"))	it-msglevel-name)
     ((equal? name (normalize "msgorig"))	it-msgorig-name)
     ((equal? name (normalize "note"))		it-note-name)
     ((equal? name (normalize "part"))		it-part-name)
     ((equal? name (normalize "preface"))	it-preface-name)
     ((equal? name (normalize "procedure"))	it-procedure-name)
     ((equal? name (normalize "pubdate"))	it-pubdate-name)
     ((equal? name (normalize "reference"))	it-reference-name)
     ((equal? name (normalize "refname"))	it-refname-name)
     ((equal? name (normalize "revhistory"))	it-revhistory-name)
     ((equal? name (normalize "revision"))	it-revision-name)
     ((equal? name (normalize "sect1"))		it-sect1-name)
     ((equal? name (normalize "sect2"))		it-sect2-name)
     ((equal? name (normalize "sect3"))		it-sect3-name)
     ((equal? name (normalize "sect4"))		it-sect4-name)
     ((equal? name (normalize "sect5"))		it-sect5-name)
     ((equal? name (normalize "simplesect"))    it-simplesect-name)
     ((equal? name (normalize "seeie"))		it-seeie-name)
     ((equal? name (normalize "seealsoie"))	it-seealsoie-name)
     ((equal? name (normalize "set"))		it-set-name)
     ((equal? name (normalize "sidebar"))	it-sidebar-name)
     ((equal? name (normalize "step"))		it-step-name)
     ((equal? name (normalize "table"))		it-table-name)
     ((equal? name (normalize "tip"))		it-tip-name)
     ((equal? name (normalize "toc"))		it-toc-name)
     ((equal? name (normalize "warning"))	it-warning-name)
     (else (let* ((msg (string-append "&unexpectedelementname;: " name))
		  (err (node-list-error msg (current-node))))
	     msg)))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-it-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;

(define it-equation-intra-label-sep "-")
(define it-example-intra-label-sep "-")
(define it-figure-intra-label-sep "-")
(define it-procedure-intra-label-sep ".")
(define it-refentry-intra-label-sep ".")
(define it-reference-intra-label-sep ".")
(define it-refname-intra-label-sep ", ")
(define it-refsect1-intra-label-sep ".")
(define it-refsect2-intra-label-sep ".")
(define it-refsect3-intra-label-sep ".")
(define it-sect1-intra-label-sep ".")
(define it-sect2-intra-label-sep ".")
(define it-sect3-intra-label-sep ".")
(define it-sect4-intra-label-sep ".")
(define it-sect5-intra-label-sep ".")
(define it-step-intra-label-sep ".")
(define it-table-intra-label-sep "-")
(define it-_pagenumber-intra-label-sep "-")
(define it-default-intra-label-sep "")

(define (gentext-it-intra-label-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "equation"))	it-equation-intra-label-sep)
     ((equal? name (normalize "example"))	it-example-intra-label-sep)
     ((equal? name (normalize "figure"))	it-figure-intra-label-sep)
     ((equal? name (normalize "procedure"))	it-procedure-intra-label-sep)
     ((equal? name (normalize "refentry"))	it-refentry-intra-label-sep)
     ((equal? name (normalize "reference"))	it-reference-intra-label-sep)
     ((equal? name (normalize "refname"))	it-refname-intra-label-sep)
     ((equal? name (normalize "refsect1"))	it-refsect1-intra-label-sep)
     ((equal? name (normalize "refsect2"))	it-refsect2-intra-label-sep)
     ((equal? name (normalize "refsect3"))	it-refsect3-intra-label-sep)
     ((equal? name (normalize "sect1"))		it-sect1-intra-label-sep)
     ((equal? name (normalize "sect2"))		it-sect2-intra-label-sep)
     ((equal? name (normalize "sect3"))		it-sect3-intra-label-sep)
     ((equal? name (normalize "sect4"))		it-sect4-intra-label-sep)
     ((equal? name (normalize "sect5"))		it-sect5-intra-label-sep)
     ((equal? name (normalize "step"))		it-step-intra-label-sep)
     ((equal? name (normalize "table"))		it-table-intra-label-sep)
     ((equal? name (normalize "_pagenumber"))	it-_pagenumber-intra-label-sep)
     (else it-default-intra-label-sep))))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;

(define it-abstract-label-title-sep ": ")
(define it-appendix-label-title-sep ". ")
(define it-caution-label-title-sep "")
(define it-chapter-label-title-sep ". ")
(define it-equation-label-title-sep ". ")
(define it-example-label-title-sep ". ")
(define it-figure-label-title-sep ". ")
(define it-footnote-label-title-sep ". ")
(define it-glosssee-label-title-sep ": ")
(define it-glossseealso-label-title-sep ": ")
(define it-important-label-title-sep ": ")
(define it-note-label-title-sep ": ")
(define it-orderedlist-label-title-sep ". ")
(define it-part-label-title-sep ". ")
(define it-procedure-label-title-sep ". ")
(define it-prefix-label-title-sep ". ")
(define it-refentry-label-title-sep "")
(define it-reference-label-title-sep ". ")
(define it-refsect1-label-title-sep ". ")
(define it-refsect2-label-title-sep ". ")
(define it-refsect3-label-title-sep ". ")
(define it-sect1-label-title-sep ". ")
(define it-sect2-label-title-sep ". ")
(define it-sect3-label-title-sep ". ")
(define it-sect4-label-title-sep ". ")
(define it-sect5-label-title-sep ". ")
(define it-seeie-label-title-sep " ")
(define it-seealsoie-label-title-sep " ")
(define it-step-label-title-sep ". ")
(define it-table-label-title-sep ". ")
(define it-tip-label-title-sep ": ")
(define it-warning-label-title-sep "")
(define it-default-label-title-sep "")

(define (gentext-it-label-title-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract")) it-abstract-label-title-sep)
     ((equal? name (normalize "appendix")) it-appendix-label-title-sep)
     ((equal? name (normalize "caution")) it-caution-label-title-sep)
     ((equal? name (normalize "chapter")) it-chapter-label-title-sep)
     ((equal? name (normalize "equation")) it-equation-label-title-sep)
     ((equal? name (normalize "example")) it-example-label-title-sep)
     ((equal? name (normalize "figure")) it-figure-label-title-sep)
     ((equal? name (normalize "footnote")) it-footnote-label-title-sep)
     ((equal? name (normalize "glosssee")) it-glosssee-label-title-sep)
     ((equal? name (normalize "glossseealso")) it-glossseealso-label-title-sep)
     ((equal? name (normalize "important")) it-important-label-title-sep)
     ((equal? name (normalize "note")) it-note-label-title-sep)
     ((equal? name (normalize "orderedlist")) it-orderedlist-label-title-sep)
     ((equal? name (normalize "part")) it-part-label-title-sep)
     ((equal? name (normalize "procedure")) it-procedure-label-title-sep)
     ((equal? name (normalize "prefix")) it-prefix-label-title-sep)
     ((equal? name (normalize "refentry")) it-refentry-label-title-sep)
     ((equal? name (normalize "reference")) it-reference-label-title-sep)
     ((equal? name (normalize "refsect1")) it-refsect1-label-title-sep)
     ((equal? name (normalize "refsect2")) it-refsect2-label-title-sep)
     ((equal? name (normalize "refsect3")) it-refsect3-label-title-sep)
     ((equal? name (normalize "sect1")) it-sect1-label-title-sep)
     ((equal? name (normalize "sect2")) it-sect2-label-title-sep)
     ((equal? name (normalize "sect3")) it-sect3-label-title-sep)
     ((equal? name (normalize "sect4")) it-sect4-label-title-sep)
     ((equal? name (normalize "sect5")) it-sect5-label-title-sep)
     ((equal? name (normalize "seeie")) it-seeie-label-title-sep)
     ((equal? name (normalize "seealsoie")) it-seealsoie-label-title-sep)
     ((equal? name (normalize "step")) it-step-label-title-sep)
     ((equal? name (normalize "table")) it-table-label-title-sep)
     ((equal? name (normalize "tip")) it-tip-label-title-sep)
     ((equal? name (normalize "warning")) it-warning-label-title-sep)
     (else it-default-label-title-sep))))

(define (it-set-label-number-format gind) "1")
(define (it-book-label-number-format gind) "1")
(define (it-prefix-label-number-format gind) "1")
(define (it-part-label-number-format gind) "I")
(define (it-chapter-label-number-format gind) "1")
(define (it-appendix-label-number-format gind) "A")
(define (it-reference-label-number-format gind) "I")
(define (it-example-label-number-format gind) "1")
(define (it-figure-label-number-format gind) "1")
(define (it-table-label-number-format gind) "1")
(define (it-procedure-label-number-format gind) "1")
(define (it-step-label-number-format gind) "1")
(define (it-refsect1-label-number-format gind) "1")
(define (it-refsect2-label-number-format gind) "1")
(define (it-refsect3-label-number-format gind) "1")
(define (it-sect1-label-number-format gind) "1")
(define (it-sect2-label-number-format gind) "1")
(define (it-sect3-label-number-format gind) "1")
(define (it-sect4-label-number-format gind) "1")
(define (it-sect5-label-number-format gind) "1")
(define (it-default-label-number-format gind) "1")

(define (it-label-number-format gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "set")) (it-set-label-number-format gind))
     ((equal? name (normalize "book")) (it-book-label-number-format gind))
     ((equal? name (normalize "prefix")) (it-prefix-label-number-format gind))
     ((equal? name (normalize "part")) (it-part-label-number-format gind))
     ((equal? name (normalize "chapter")) (it-chapter-label-number-format gind))
     ((equal? name (normalize "appendix")) (it-appendix-label-number-format gind))
     ((equal? name (normalize "reference")) (it-reference-label-number-format gind))
     ((equal? name (normalize "example")) (it-example-label-number-format gind))
     ((equal? name (normalize "figure")) (it-figure-label-number-format gind))
     ((equal? name (normalize "table")) (it-table-label-number-format gind))
     ((equal? name (normalize "procedure")) (it-procedure-label-number-format gind))
     ((equal? name (normalize "step")) (it-step-label-number-format gind))
     ((equal? name (normalize "refsect1")) (it-refsect1-label-number-format gind))
     ((equal? name (normalize "refsect2")) (it-refsect2-label-number-format gind))
     ((equal? name (normalize "refsect3")) (it-refsect3-label-number-format gind))
     ((equal? name (normalize "sect1")) (it-sect1-label-number-format gind))
     ((equal? name (normalize "sect2")) (it-sect2-label-number-format gind))
     ((equal? name (normalize "sect3")) (it-sect3-label-number-format gind))
     ((equal? name (normalize "sect4")) (it-sect4-label-number-format gind))
     ((equal? name (normalize "sect5")) (it-sect5-label-number-format gind))
     (else (it-default-label-number-format gind)))))

(define ($lot-title-it$ gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond ((equal? name (normalize "table"))    "&ListofTables;")
	  ((equal? name (normalize "example"))  "&ListofExamples;")
	  ((equal? name (normalize "figure"))   "&ListofFigures;")
	  ((equal? name (normalize "equation")) "&ListofEquations;")
	  (else (let* ((msg (string-append "&ListofUnknown;: " name))
		       (err (node-list-error msg (current-node))))
		  msg)))))

(define %gentext-it-start-quote% (dingbat "ldquo"))

(define %gentext-it-end-quote% (dingbat "rdquo"))

(define %gentext-it-by% "") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-it-edited-by% "&Editedby;")
                           ;; "Edited by" Jane Doe

(define %gentext-it-page% "")

(define %gentext-it-and% "&and;")

(define %gentext-it-bibl-pages% "&Pgs;")

(define %gentext-it-endnotes% "&Notes;")

(define %gentext-it-table-endnotes% "&TableNotes;:")

(define %gentext-it-index-see% "&See;")

(define %gentext-it-index-seealso% "&SeeAlso;")

