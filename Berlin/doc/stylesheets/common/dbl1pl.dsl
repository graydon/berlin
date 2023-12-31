;; $Id: dbl1pl.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; ----------------------------- Localization -----------------------------

;; Polish localization.

;; If you create a new version of this file, please send it to
;; Norman Walsh, ndw@nwalsh.com

;; The generated text for cross references to elements.  See dblink.dsl
;; for a discussion of how substitution is performed on the %x
;; keywords.
;;

(define (pl-appendix-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Appendix; %n"
      "dodatek %t"))

(define (pl-article-xref-string gi-or-name)
  (string-append %gentext-pl-start-quote%
		 "%t"
		 %gentext-pl-end-quote%))

(define (pl-bibliography-xref-string gi-or-name)
  "%t")

(define (pl-book-xref-string gi-or-name)
  "%t")

(define (pl-chapter-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Chapter; %n"
      "rozdzia&#179; %t"))

(define (pl-equation-xref-string gi-or-name)
  "&Equation; %n")

(define (pl-example-xref-string gi-or-name)
  "&Example; %n")

(define (pl-figure-xref-string gi-or-name)
  "&Figure; %n")

(define (pl-listitem-xref-string gi-or-name)
  "%n")

(define (pl-part-xref-string gi-or-name)
  "&Part; %n")

(define (pl-preface-xref-string gi-or-name)
  "%t")

(define (pl-procedure-xref-string gi-or-name)
  "&Procedure; %n, %t")

(define (pl-section-xref-string gi-or-name)
  (if %section-autolabel% 
      "&Section; %n" 
      "sekcja %t"))

(define (pl-sect1-xref-string gi-or-name)
  (pl-section-xref-string gi-or-name))

(define (pl-sect2-xref-string gi-or-name)
  (pl-section-xref-string gi-or-name))

(define (pl-sect3-xref-string gi-or-name)
  (pl-section-xref-string gi-or-name))

(define (pl-sect4-xref-string gi-or-name)
  (pl-section-xref-string gi-or-name))

(define (pl-sect5-xref-string gi-or-name)
  (pl-section-xref-string gi-or-name))

(define (pl-step-xref-string gi-or-name)
  "&Step; %n")

(define (pl-table-xref-string gi-or-name)
  "&Table; %n")

(define (pl-default-xref-string gi-or-name)
  (let* ((giname (if (string? gi-or-name) gi-or-name (gi gi-or-name)))
	 (msg    (string-append "[&xrefto; "
				(if giname giname "&nonexistantelement;")
				" &unsupported;]"))
	 (err    (node-list-error msg (current-node))))
    msg))

(define (gentext-pl-xref-strings gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
      ((equal? name (normalize "appendix")) (pl-appendix-xref-string gind))
      ((equal? name (normalize "article"))  (pl-article-xref-string gind))
      ((equal? name (normalize "bibliography")) (pl-bibliography-xref-string gind))
      ((equal? name (normalize "book"))     (pl-book-xref-string gind))
      ((equal? name (normalize "chapter"))  (pl-chapter-xref-string gind))
      ((equal? name (normalize "equation")) (pl-equation-xref-string gind))
      ((equal? name (normalize "example"))  (pl-example-xref-string gind))
      ((equal? name (normalize "figure"))   (pl-figure-xref-string gind))
      ((equal? name (normalize "listitem")) (pl-listitem-xref-string gind))
      ((equal? name (normalize "part"))     (pl-part-xref-string gind))
      ((equal? name (normalize "preface"))  (pl-preface-xref-string gind))
      ((equal? name (normalize "procedure")) (pl-procedure-xref-string gind))
      ((equal? name (normalize "sect1"))    (pl-sect1-xref-string gind))
      ((equal? name (normalize "sect2"))    (pl-sect2-xref-string gind))
      ((equal? name (normalize "sect3"))    (pl-sect3-xref-string gind))
      ((equal? name (normalize "sect4"))    (pl-sect4-xref-string gind))
      ((equal? name (normalize "sect5"))    (pl-sect5-xref-string gind))
      ((equal? name (normalize "step"))     (pl-step-xref-string gind))
      ((equal? name (normalize "table"))    (pl-table-xref-string gind))
      (else (pl-default-xref-string gind)))))

(define (pl-auto-xref-indirect-connector before) 
  (literal " &in; "))

;; Should the TOC come first or last?
;;
(define %generate-pl-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define fr-abstract-name	"&Abstract;")
(define fr-appendix-name	"&Appendix;")
(define fr-article-name	"&Article;")
(define fr-bibliography-name	"&Bibliography;")
(define fr-book-name		"&Book;")
(define fr-calloutlist-name   "")
(define fr-caution-name	"&Caution;")
(define fr-chapter-name	"&Chapter;")
(define fr-copyright-name	"&Copyright;")
(define fr-dedication-name	"&Dedication;")
(define fr-edition-name	"&Edition;")
(define fr-equation-name	"&Equation;")
(define fr-example-name	"&Example;")
(define fr-figure-name	"&Figure;")
(define fr-glossary-name	"&Glossary;")
(define fr-glosssee-name	"&GlossSee;")
(define fr-glossseealso-name	"&GlossSeeAlso;")
(define fr-important-name	"&Important;")
(define fr-index-name		"&Index;")
(define fr-setindex-name	"&SetIndex;")
(define fr-isbn-name		"&ISBN;")
(define fr-legalnotice-name	"")
(define fr-msgaud-name	"&MsgAud;")
(define fr-msglevel-name	"&MsgLevel;")
(define fr-msgorig-name	"&MsgOrig;")
(define fr-note-name		"&Note;")
(define fr-part-name		"&Part;")
(define fr-preface-name	"&Preface;")
(define fr-procedure-name	"&Procedure;")
(define fr-pubdate-name	"&Published;")
(define fr-reference-name	"&Reference;")
(define fr-refname-name	"&RefName;")
(define fr-revhistory-name	"&RevHistory;")
(define fr-revision-name	"&Revision;")
(define fr-sect1-name		"&Section;")
(define fr-sect2-name		"&Section;")
(define fr-sect3-name		"&Section;")
(define fr-sect4-name		"&Section;")
(define fr-sect5-name		"&Section;")
(define fr-simplesect-name	"&Section;")
(define fr-seeie-name		"&See;")
(define fr-seealsoie-name	"&Seealso;")
(define fr-set-name		"&Set;")
(define fr-sidebar-name	"&Sidebar;")
(define fr-step-name		"&step;")
(define fr-table-name		"&Table;")
(define fr-tip-name		"&Tip;")
(define fr-toc-name		"&TableofContents;")
(define fr-warning-name	"&Warning;")

(define (gentext-pl-element-name gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract"))	fr-abstract-name)
     ((equal? name (normalize "appendix"))	fr-appendix-name)
     ((equal? name (normalize "article"))	fr-article-name)
     ((equal? name (normalize "bibliography"))	fr-bibliography-name)
     ((equal? name (normalize "book"))		fr-book-name)
     ((equal? name (normalize "calloutlist"))	fr-calloutlist-name)
     ((equal? name (normalize "caution"))	fr-caution-name)
     ((equal? name (normalize "chapter"))	fr-chapter-name)
     ((equal? name (normalize "copyright"))	fr-copyright-name)
     ((equal? name (normalize "dedication"))	fr-dedication-name)
     ((equal? name (normalize "edition"))	fr-edition-name)
     ((equal? name (normalize "equation"))	fr-equation-name)
     ((equal? name (normalize "example"))	fr-example-name)
     ((equal? name (normalize "figure"))	fr-figure-name)
     ((equal? name (normalize "glossary"))	fr-glossary-name)
     ((equal? name (normalize "glosssee"))	fr-glosssee-name)
     ((equal? name (normalize "glossseealso"))	fr-glossseealso-name)
     ((equal? name (normalize "important"))	fr-important-name)
     ((equal? name (normalize "index"))		fr-index-name)
     ((equal? name (normalize "setindex"))	fr-setindex-name)
     ((equal? name (normalize "isbn"))		fr-isbn-name)
     ((equal? name (normalize "legalnotice"))	fr-legalnotice-name)
     ((equal? name (normalize "msgaud"))	fr-msgaud-name)
     ((equal? name (normalize "msglevel"))	fr-msglevel-name)
     ((equal? name (normalize "msgorig"))	fr-msgorig-name)
     ((equal? name (normalize "note"))		fr-note-name)
     ((equal? name (normalize "part"))		fr-part-name)
     ((equal? name (normalize "preface"))	fr-preface-name)
     ((equal? name (normalize "procedure"))	fr-procedure-name)
     ((equal? name (normalize "pubdate"))	fr-pubdate-name)
     ((equal? name (normalize "reference"))	fr-reference-name)
     ((equal? name (normalize "refname"))	fr-refname-name)
     ((equal? name (normalize "revhistory"))	fr-revhistory-name)
     ((equal? name (normalize "revision"))	fr-revision-name)
     ((equal? name (normalize "sect1"))		fr-sect1-name)
     ((equal? name (normalize "sect2"))		fr-sect2-name)
     ((equal? name (normalize "sect3"))		fr-sect3-name)
     ((equal? name (normalize "sect4"))		fr-sect4-name)
     ((equal? name (normalize "sect5"))		fr-sect5-name)
     ((equal? name (normalize "simplesect"))	fr-simplesect-name)
     ((equal? name (normalize "seeie"))		fr-seeie-name)
     ((equal? name (normalize "seealsoie"))	fr-seealsoie-name)
     ((equal? name (normalize "set"))		fr-set-name)
     ((equal? name (normalize "sidebar"))	fr-sidebar-name)
     ((equal? name (normalize "step"))		fr-step-name)
     ((equal? name (normalize "table"))		fr-table-name)
     ((equal? name (normalize "tip"))		fr-tip-name)
     ((equal? name (normalize "toc"))		fr-toc-name)
     ((equal? name (normalize "warning"))	fr-warning-name)
     (else (let* ((msg (string-append "&unexpectedelementname;: " name))
		  (err (node-list-error msg (current-node))))
	     msg)))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-pl-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;

(define fr-equation-intra-label-sep "-")
(define fr-example-intra-label-sep "-")
(define fr-figure-intra-label-sep "-")
(define fr-procedure-intra-label-sep ".")
(define fr-refentry-intra-label-sep ".")
(define fr-reference-intra-label-sep ".")
(define fr-refname-intra-label-sep ", ")
(define fr-refsect1-intra-label-sep ".")
(define fr-refsect2-intra-label-sep ".")
(define fr-refsect3-intra-label-sep ".")
(define fr-sect1-intra-label-sep ".")
(define fr-sect2-intra-label-sep ".")
(define fr-sect3-intra-label-sep ".")
(define fr-sect4-intra-label-sep ".")
(define fr-sect5-intra-label-sep ".")
(define fr-step-intra-label-sep ".")
(define fr-table-intra-label-sep "-")
(define fr-_pagenumber-intra-label-sep "-")
(define fr-default-intra-label-sep "")

(define (gentext-pl-intra-label-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "equation"))	fr-equation-intra-label-sep)
     ((equal? name (normalize "example"))	fr-example-intra-label-sep)
     ((equal? name (normalize "figure"))	fr-figure-intra-label-sep)
     ((equal? name (normalize "procedure"))	fr-procedure-intra-label-sep)
     ((equal? name (normalize "refentry"))	fr-refentry-intra-label-sep)
     ((equal? name (normalize "reference"))	fr-reference-intra-label-sep)
     ((equal? name (normalize "refname"))	fr-refname-intra-label-sep)
     ((equal? name (normalize "refsect1"))	fr-refsect1-intra-label-sep)
     ((equal? name (normalize "refsect2"))	fr-refsect2-intra-label-sep)
     ((equal? name (normalize "refsect3"))	fr-refsect3-intra-label-sep)
     ((equal? name (normalize "sect1"))		fr-sect1-intra-label-sep)
     ((equal? name (normalize "sect2"))		fr-sect2-intra-label-sep)
     ((equal? name (normalize "sect3"))		fr-sect3-intra-label-sep)
     ((equal? name (normalize "sect4"))		fr-sect4-intra-label-sep)
     ((equal? name (normalize "sect5"))		fr-sect5-intra-label-sep)
     ((equal? name (normalize "step"))		fr-step-intra-label-sep)
     ((equal? name (normalize "table"))		fr-table-intra-label-sep)
     ((equal? name (normalize "_pagenumber"))	fr-_pagenumber-intra-label-sep)
     (else fr-default-intra-label-sep))))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;

(define fr-abstract-label-title-sep ": ")
(define fr-appendix-label-title-sep ". ")
(define fr-caution-label-title-sep "")
(define fr-chapter-label-title-sep ". ")
(define fr-equation-label-title-sep ". ")
(define fr-example-label-title-sep ". ")
(define fr-figure-label-title-sep ". ")
(define fr-footnote-label-title-sep ". ")
(define fr-glosssee-label-title-sep ": ")
(define fr-glossseealso-label-title-sep ": ")
(define fr-important-label-title-sep ": ")
(define fr-note-label-title-sep ": ")
(define fr-orderedlist-label-title-sep ". ")
(define fr-part-label-title-sep ". ")
(define fr-procedure-label-title-sep ". ")
(define fr-prefix-label-title-sep ". ")
(define fr-refentry-label-title-sep "")
(define fr-reference-label-title-sep ". ")
(define fr-refsect1-label-title-sep ". ")
(define fr-refsect2-label-title-sep ". ")
(define fr-refsect3-label-title-sep ". ")
(define fr-sect1-label-title-sep ". ")
(define fr-sect2-label-title-sep ". ")
(define fr-sect3-label-title-sep ". ")
(define fr-sect4-label-title-sep ". ")
(define fr-sect5-label-title-sep ". ")
(define fr-seeie-label-title-sep " ")
(define fr-seealsoie-label-title-sep " ")
(define fr-step-label-title-sep ". ")
(define fr-table-label-title-sep ". ")
(define fr-tip-label-title-sep ": ")
(define fr-warning-label-title-sep "")
(define fr-default-label-title-sep "")

(define (gentext-pl-label-title-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract")) fr-abstract-label-title-sep)
     ((equal? name (normalize "appendix")) fr-appendix-label-title-sep)
     ((equal? name (normalize "caution")) fr-caution-label-title-sep)
     ((equal? name (normalize "chapter")) fr-chapter-label-title-sep)
     ((equal? name (normalize "equation")) fr-equation-label-title-sep)
     ((equal? name (normalize "example")) fr-example-label-title-sep)
     ((equal? name (normalize "figure")) fr-figure-label-title-sep)
     ((equal? name (normalize "footnote")) fr-footnote-label-title-sep)
     ((equal? name (normalize "glosssee")) fr-glosssee-label-title-sep)
     ((equal? name (normalize "glossseealso")) fr-glossseealso-label-title-sep)
     ((equal? name (normalize "important")) fr-important-label-title-sep)
     ((equal? name (normalize "note")) fr-note-label-title-sep)
     ((equal? name (normalize "orderedlist")) fr-orderedlist-label-title-sep)
     ((equal? name (normalize "part")) fr-part-label-title-sep)
     ((equal? name (normalize "procedure")) fr-procedure-label-title-sep)
     ((equal? name (normalize "prefix")) fr-prefix-label-title-sep)
     ((equal? name (normalize "refentry")) fr-refentry-label-title-sep)
     ((equal? name (normalize "reference")) fr-reference-label-title-sep)
     ((equal? name (normalize "refsect1")) fr-refsect1-label-title-sep)
     ((equal? name (normalize "refsect2")) fr-refsect2-label-title-sep)
     ((equal? name (normalize "refsect3")) fr-refsect3-label-title-sep)
     ((equal? name (normalize "sect1")) fr-sect1-label-title-sep)
     ((equal? name (normalize "sect2")) fr-sect2-label-title-sep)
     ((equal? name (normalize "sect3")) fr-sect3-label-title-sep)
     ((equal? name (normalize "sect4")) fr-sect4-label-title-sep)
     ((equal? name (normalize "sect5")) fr-sect5-label-title-sep)
     ((equal? name (normalize "seeie")) fr-seeie-label-title-sep)
     ((equal? name (normalize "seealsoie")) fr-seealsoie-label-title-sep)
     ((equal? name (normalize "step")) fr-step-label-title-sep)
     ((equal? name (normalize "table")) fr-table-label-title-sep)
     ((equal? name (normalize "tip")) fr-tip-label-title-sep)
     ((equal? name (normalize "warning")) fr-warning-label-title-sep)
     (else fr-default-label-title-sep))))

(define (pl-set-label-number-format gind) "1")
(define (pl-book-label-number-format gind) "1")
(define (pl-prefix-label-number-format gind) "1")
(define (pl-part-label-number-format gind) "I")
(define (pl-chapter-label-number-format gind) "1")
(define (pl-appendix-label-number-format gind) "A")
(define (pl-reference-label-number-format gind) "I")
(define (pl-example-label-number-format gind) "1")
(define (pl-figure-label-number-format gind) "1")
(define (pl-table-label-number-format gind) "1")
(define (pl-procedure-label-number-format gind) "1")
(define (pl-step-label-number-format gind) "1")
(define (pl-refsect1-label-number-format gind) "1")
(define (pl-refsect2-label-number-format gind) "1")
(define (pl-refsect3-label-number-format gind) "1")
(define (pl-sect1-label-number-format gind) "1")
(define (pl-sect2-label-number-format gind) "1")
(define (pl-sect3-label-number-format gind) "1")
(define (pl-sect4-label-number-format gind) "1")
(define (pl-sect5-label-number-format gind) "1")
(define (pl-default-label-number-format gind) "1")

(define (pl-label-number-format gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "set")) (pl-set-label-number-format gind))
     ((equal? name (normalize "book")) (pl-book-label-number-format gind))
     ((equal? name (normalize "prefix")) (pl-prefix-label-number-format gind))
     ((equal? name (normalize "part")) (pl-part-label-number-format gind))
     ((equal? name (normalize "chapter")) (pl-chapter-label-number-format gind))
     ((equal? name (normalize "appendix")) (pl-appendix-label-number-format gind))
     ((equal? name (normalize "reference")) (pl-reference-label-number-format gind))
     ((equal? name (normalize "example")) (pl-example-label-number-format gind))
     ((equal? name (normalize "figure")) (pl-figure-label-number-format gind))
     ((equal? name (normalize "table")) (pl-table-label-number-format gind))
     ((equal? name (normalize "procedure")) (pl-procedure-label-number-format gind))
     ((equal? name (normalize "step")) (pl-step-label-number-format gind))
     ((equal? name (normalize "refsect1")) (pl-refsect1-label-number-format gind))
     ((equal? name (normalize "refsect2")) (pl-refsect2-label-number-format gind))
     ((equal? name (normalize "refsect3")) (pl-refsect3-label-number-format gind))
     ((equal? name (normalize "sect1")) (pl-sect1-label-number-format gind))
     ((equal? name (normalize "sect2")) (pl-sect2-label-number-format gind))
     ((equal? name (normalize "sect3")) (pl-sect3-label-number-format gind))
     ((equal? name (normalize "sect4")) (pl-sect4-label-number-format gind))
     ((equal? name (normalize "sect5")) (pl-sect5-label-number-format gind))
     (else (pl-default-label-number-format gind)))))

(define ($lot-title-pl$ gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond ((equal? name (normalize "table"))    "&ListofTables;")
	  ((equal? name (normalize "example"))  "&ListofExamples;")
	  ((equal? name (normalize "figure"))   "&ListofFigures;")
	  ((equal? name (normalize "equation")) "&ListofEquations;")
	  (else (let* ((msg (string-append "&ListofUnknown;: " name))
		       (err (node-list-error msg (current-node))))
		  msg)))))

(define %gentext-pl-start-quote%  (dingbat "ldquo"))

(define %gentext-pl-end-quote%  (dingbat "rdquo"))

(define %gentext-pl-by% "&by;") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-pl-edited-by% "&Editedby;")
                           ;; "Edited by" Jane Doe

(define %gentext-pl-page% "")

(define %gentext-pl-and% "&and;")

(define %gentext-pl-bibl-pages% "&Pgs;")

(define %gentext-pl-endnotes% "&Notes;")

(define %gentext-pl-table-endnotes% "&TableNotes;:")

(define %gentext-pl-index-see% "&See;")

(define %gentext-pl-index-seealso% "&SeeAlso;")
