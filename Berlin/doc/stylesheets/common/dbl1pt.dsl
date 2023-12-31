;; $Id: dbl1pt.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
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

(define (pt-appendix-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Appendix; %n"
      "o &appendix; %t"))

(define (pt-article-xref-string gi-or-name)
  (string-append %gentext-pt-start-quote%
		 "%t"
		 %gentext-pt-end-quote%))

(define (pt-bibliography-xref-string gi-or-name)
  "%t")

(define (pt-book-xref-string gi-or-name)
  "%t")

(define (pt-chapter-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Chapter; %n"
      "o &chapter; %t"))

(define (pt-equation-xref-string gi-or-name)
  "&Equation; %n")

(define (pt-example-xref-string gi-or-name)
  "&Example; %n")

(define (pt-figure-xref-string gi-or-name)
  "&Figure; %n")

(define (pt-listitem-xref-string gi-or-name)
  "%n")

(define (pt-part-xref-string gi-or-name)
  "&Part; %n")

(define (pt-preface-xref-string gi-or-name)
  "%t")

(define (pt-procedure-xref-string gi-or-name)
  "&Procedure; %n, %t")

(define (pt-section-xref-string gi-or-name)
  (if %section-autolabel% 
      "&Section; %n" 
      "o &section; %t"))

(define (pt-sect1-xref-string gi-or-name)
  (pt-section-xref-string gi-or-name))

(define (pt-sect2-xref-string gi-or-name)
  (pt-section-xref-string gi-or-name))

(define (pt-sect3-xref-string gi-or-name)
  (pt-section-xref-string gi-or-name))

(define (pt-sect4-xref-string gi-or-name)
  (pt-section-xref-string gi-or-name))

(define (pt-sect5-xref-string gi-or-name)
  (pt-section-xref-string gi-or-name))

(define (pt-step-xref-string gi-or-name)
  "&step; %n")

(define (pt-table-xref-string gi-or-name)
  "&Table; %n")

(define (pt-default-xref-string gi-or-name)
  (let* ((giname (if (string? gi-or-name) gi-or-name (gi gi-or-name)))
	 (msg    (string-append "[&xrefto; "
				(if giname giname "&nonexistantelement;")
				" &unsupported;]"))
	 (err    (node-list-error msg (current-node))))
    msg))

(define (gentext-pt-xref-strings gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
      ((equal? name (normalize "appendix")) (pt-appendix-xref-string gind))
      ((equal? name (normalize "article"))  (pt-article-xref-string gind))
      ((equal? name (normalize "bibliography")) (pt-bibliography-xref-string gind))
      ((equal? name (normalize "book"))     (pt-book-xref-string gind))
      ((equal? name (normalize "chapter"))  (pt-chapter-xref-string gind))
      ((equal? name (normalize "equation")) (pt-equation-xref-string gind))
      ((equal? name (normalize "example"))  (pt-example-xref-string gind))
      ((equal? name (normalize "figure"))   (pt-figure-xref-string gind))
      ((equal? name (normalize "listitem")) (pt-listitem-xref-string gind))
      ((equal? name (normalize "part"))     (pt-part-xref-string gind))
      ((equal? name (normalize "preface"))  (pt-preface-xref-string gind))
      ((equal? name (normalize "procedure")) (pt-procedure-xref-string gind))
      ((equal? name (normalize "sect1"))    (pt-sect1-xref-string gind))
      ((equal? name (normalize "sect2"))    (pt-sect2-xref-string gind))
      ((equal? name (normalize "sect3"))    (pt-sect3-xref-string gind))
      ((equal? name (normalize "sect4"))    (pt-sect4-xref-string gind))
      ((equal? name (normalize "sect5"))    (pt-sect5-xref-string gind))
      ((equal? name (normalize "step"))     (pt-step-xref-string gind))
      ((equal? name (normalize "table"))    (pt-table-xref-string gind))
      (else (pt-default-xref-string gind)))))

(define (pt-auto-xref-indirect-connector before) 
  (literal " &in; "))

;; Should the TOC come first or last?
;;
(define %generate-pt-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define pt-abstract-name	"&Abstract;")
(define pt-appendix-name	"&Appendix;")
(define pt-article-name	"&Article;")
(define pt-bibliography-name	"&Bibliography;")
(define pt-book-name		"&Book;")
(define pt-calloutlist-name	"")
(define pt-caution-name	"&Caution;")
(define pt-chapter-name	"&Chapter;")
(define pt-copyright-name	"&Copyright;")
(define pt-dedication-name	"&Dedication;")
(define pt-edition-name	"&Edition;")
(define pt-equation-name	"&Equation;")
(define pt-example-name	"&Example;")
(define pt-figure-name	"&Figure;")
(define pt-glossary-name	"&Glossary;")
(define pt-glosssee-name	"&GlossSee;")
(define pt-glossseealso-name	"&GlossSeeAlso;")
(define pt-important-name	"&Important;")
(define pt-index-name		"&Index;")
(define pt-setindex-name	"&SetIndex;")
(define pt-isbn-name		"&ISBN;")
(define pt-legalnotice-name	"&LegalNotice;")
(define pt-msgaud-name	"&MsgAud;")
(define pt-msglevel-name	"&MsgLevel;")
(define pt-msgorig-name	"&MsgOrig;")
(define pt-note-name		"&Note;")
(define pt-part-name		"&Part;")
(define pt-preface-name	"&Preface;")
(define pt-procedure-name	"&Procedure;")
(define pt-pubdate-name	"&Published;")
(define pt-reference-name	"&Reference;")
(define pt-refname-name	"&RefName;")
(define pt-revhistory-name	"&RevHistory;")
(define pt-revision-name	"&Revision;")
(define pt-sect1-name		"&Section;")
(define pt-sect2-name		"&Section;")
(define pt-sect3-name		"&Section;")
(define pt-sect4-name		"&Section;")
(define pt-sect5-name		"&Section;")
(define pt-simplesect-name	"&Section;")
(define pt-seeie-name		"&See;")
(define pt-seealsoie-name	"&Seealso;")
(define pt-set-name		"&Set;")
(define pt-sidebar-name	"&Sidebar;")
(define pt-step-name		"&step;")
(define pt-table-name		"&Table;")
(define pt-tip-name		"&Tip;")
(define pt-toc-name		"&TableofContents;")
(define pt-warning-name	"&Warning;")

(define (gentext-pt-element-name gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract"))	pt-abstract-name)
     ((equal? name (normalize "appendix"))	pt-appendix-name)
     ((equal? name (normalize "article"))	pt-article-name)
     ((equal? name (normalize "bibliography"))	pt-bibliography-name)
     ((equal? name (normalize "book"))		pt-book-name)
     ((equal? name (normalize "calloutlist"))	pt-calloutlist-name)
     ((equal? name (normalize "caution"))	pt-caution-name)
     ((equal? name (normalize "chapter"))	pt-chapter-name)
     ((equal? name (normalize "copyright"))	pt-copyright-name)
     ((equal? name (normalize "dedication"))	pt-dedication-name)
     ((equal? name (normalize "edition"))	pt-edition-name)
     ((equal? name (normalize "equation"))	pt-equation-name)
     ((equal? name (normalize "example"))	pt-example-name)
     ((equal? name (normalize "figure"))	pt-figure-name)
     ((equal? name (normalize "glossary"))	pt-glossary-name)
     ((equal? name (normalize "glosssee"))	pt-glosssee-name)
     ((equal? name (normalize "glossseealso"))	pt-glossseealso-name)
     ((equal? name (normalize "important"))	pt-important-name)
     ((equal? name (normalize "index"))		pt-index-name)
     ((equal? name (normalize "setindex"))	pt-setindex-name)
     ((equal? name (normalize "isbn"))		pt-isbn-name)
     ((equal? name (normalize "legalnotice"))	pt-legalnotice-name)
     ((equal? name (normalize "msgaud"))	pt-msgaud-name)
     ((equal? name (normalize "msglevel"))	pt-msglevel-name)
     ((equal? name (normalize "msgorig"))	pt-msgorig-name)
     ((equal? name (normalize "note"))		pt-note-name)
     ((equal? name (normalize "part"))		pt-part-name)
     ((equal? name (normalize "preface"))	pt-preface-name)
     ((equal? name (normalize "procedure"))	pt-procedure-name)
     ((equal? name (normalize "pubdate"))	pt-pubdate-name)
     ((equal? name (normalize "reference"))	pt-reference-name)
     ((equal? name (normalize "refname"))	pt-refname-name)
     ((equal? name (normalize "revhistory"))	pt-revhistory-name)
     ((equal? name (normalize "revision"))	pt-revision-name)
     ((equal? name (normalize "sect1"))		pt-sect1-name)
     ((equal? name (normalize "sect2"))		pt-sect2-name)
     ((equal? name (normalize "sect3"))		pt-sect3-name)
     ((equal? name (normalize "sect4"))		pt-sect4-name)
     ((equal? name (normalize "sect5"))		pt-sect5-name)
     ((equal? name (normalize "simplesect"))    pt-simplesect-name)
     ((equal? name (normalize "seeie"))		pt-seeie-name)
     ((equal? name (normalize "seealsoie"))	pt-seealsoie-name)
     ((equal? name (normalize "set"))		pt-set-name)
     ((equal? name (normalize "sidebar"))	pt-sidebar-name)
     ((equal? name (normalize "step"))		pt-step-name)
     ((equal? name (normalize "table"))		pt-table-name)
     ((equal? name (normalize "tip"))		pt-tip-name)
     ((equal? name (normalize "toc"))		pt-toc-name)
     ((equal? name (normalize "warning"))	pt-warning-name)
     (else (let* ((msg (string-append "&unexpectedelementname;: " name))
		  (err (node-list-error msg (current-node))))
	     msg)))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-pt-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;

(define pt-equation-intra-label-sep "-")
(define pt-example-intra-label-sep "-")
(define pt-figure-intra-label-sep "-")
(define pt-procedure-intra-label-sep ".")
(define pt-refentry-intra-label-sep ".")
(define pt-reference-intra-label-sep ".")
(define pt-refname-intra-label-sep ", ")
(define pt-refsect1-intra-label-sep ".")
(define pt-refsect2-intra-label-sep ".")
(define pt-refsect3-intra-label-sep ".")
(define pt-sect1-intra-label-sep ".")
(define pt-sect2-intra-label-sep ".")
(define pt-sect3-intra-label-sep ".")
(define pt-sect4-intra-label-sep ".")
(define pt-sect5-intra-label-sep ".")
(define pt-step-intra-label-sep ".")
(define pt-table-intra-label-sep "-")
(define pt-_pagenumber-intra-label-sep "-")
(define pt-default-intra-label-sep "")

(define (gentext-pt-intra-label-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "equation"))	pt-equation-intra-label-sep)
     ((equal? name (normalize "example"))	pt-example-intra-label-sep)
     ((equal? name (normalize "figure"))	pt-figure-intra-label-sep)
     ((equal? name (normalize "procedure"))	pt-procedure-intra-label-sep)
     ((equal? name (normalize "refentry"))	pt-refentry-intra-label-sep)
     ((equal? name (normalize "reference"))	pt-reference-intra-label-sep)
     ((equal? name (normalize "refname"))	pt-refname-intra-label-sep)
     ((equal? name (normalize "refsect1"))	pt-refsect1-intra-label-sep)
     ((equal? name (normalize "refsect2"))	pt-refsect2-intra-label-sep)
     ((equal? name (normalize "refsect3"))	pt-refsect3-intra-label-sep)
     ((equal? name (normalize "sect1"))		pt-sect1-intra-label-sep)
     ((equal? name (normalize "sect2"))		pt-sect2-intra-label-sep)
     ((equal? name (normalize "sect3"))		pt-sect3-intra-label-sep)
     ((equal? name (normalize "sect4"))		pt-sect4-intra-label-sep)
     ((equal? name (normalize "sect5"))		pt-sect5-intra-label-sep)
     ((equal? name (normalize "step"))		pt-step-intra-label-sep)
     ((equal? name (normalize "table"))		pt-table-intra-label-sep)
     ((equal? name (normalize "_pagenumber"))	pt-_pagenumber-intra-label-sep)
     (else pt-default-intra-label-sep))))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;

(define pt-abstract-label-title-sep ": ")
(define pt-appendix-label-title-sep ". ")
(define pt-caution-label-title-sep "")
(define pt-chapter-label-title-sep ". ")
(define pt-equation-label-title-sep ". ")
(define pt-example-label-title-sep ". ")
(define pt-figure-label-title-sep ". ")
(define pt-footnote-label-title-sep ". ")
(define pt-glosssee-label-title-sep ": ")
(define pt-glossseealso-label-title-sep ": ")
(define pt-important-label-title-sep ": ")
(define pt-note-label-title-sep ": ")
(define pt-orderedlist-label-title-sep ". ")
(define pt-part-label-title-sep ". ")
(define pt-procedure-label-title-sep ". ")
(define pt-prefix-label-title-sep ". ")
(define pt-refentry-label-title-sep "")
(define pt-reference-label-title-sep ". ")
(define pt-refsect1-label-title-sep ". ")
(define pt-refsect2-label-title-sep ". ")
(define pt-refsect3-label-title-sep ". ")
(define pt-sect1-label-title-sep ". ")
(define pt-sect2-label-title-sep ". ")
(define pt-sect3-label-title-sep ". ")
(define pt-sect4-label-title-sep ". ")
(define pt-sect5-label-title-sep ". ")
(define pt-seeie-label-title-sep " ")
(define pt-seealsoie-label-title-sep " ")
(define pt-step-label-title-sep ". ")
(define pt-table-label-title-sep ". ")
(define pt-tip-label-title-sep ": ")
(define pt-warning-label-title-sep "")
(define pt-default-label-title-sep "")

(define (gentext-pt-label-title-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract")) pt-abstract-label-title-sep)
     ((equal? name (normalize "appendix")) pt-appendix-label-title-sep)
     ((equal? name (normalize "caution")) pt-caution-label-title-sep)
     ((equal? name (normalize "chapter")) pt-chapter-label-title-sep)
     ((equal? name (normalize "equation")) pt-equation-label-title-sep)
     ((equal? name (normalize "example")) pt-example-label-title-sep)
     ((equal? name (normalize "figure")) pt-figure-label-title-sep)
     ((equal? name (normalize "footnote")) pt-footnote-label-title-sep)
     ((equal? name (normalize "glosssee")) pt-glosssee-label-title-sep)
     ((equal? name (normalize "glossseealso")) pt-glossseealso-label-title-sep)
     ((equal? name (normalize "important")) pt-important-label-title-sep)
     ((equal? name (normalize "note")) pt-note-label-title-sep)
     ((equal? name (normalize "orderedlist")) pt-orderedlist-label-title-sep)
     ((equal? name (normalize "part")) pt-part-label-title-sep)
     ((equal? name (normalize "procedure")) pt-procedure-label-title-sep)
     ((equal? name (normalize "prefix")) pt-prefix-label-title-sep)
     ((equal? name (normalize "refentry")) pt-refentry-label-title-sep)
     ((equal? name (normalize "reference")) pt-reference-label-title-sep)
     ((equal? name (normalize "refsect1")) pt-refsect1-label-title-sep)
     ((equal? name (normalize "refsect2")) pt-refsect2-label-title-sep)
     ((equal? name (normalize "refsect3")) pt-refsect3-label-title-sep)
     ((equal? name (normalize "sect1")) pt-sect1-label-title-sep)
     ((equal? name (normalize "sect2")) pt-sect2-label-title-sep)
     ((equal? name (normalize "sect3")) pt-sect3-label-title-sep)
     ((equal? name (normalize "sect4")) pt-sect4-label-title-sep)
     ((equal? name (normalize "sect5")) pt-sect5-label-title-sep)
     ((equal? name (normalize "seeie")) pt-seeie-label-title-sep)
     ((equal? name (normalize "seealsoie")) pt-seealsoie-label-title-sep)
     ((equal? name (normalize "step")) pt-step-label-title-sep)
     ((equal? name (normalize "table")) pt-table-label-title-sep)
     ((equal? name (normalize "tip")) pt-tip-label-title-sep)
     ((equal? name (normalize "warning")) pt-warning-label-title-sep)
     (else pt-default-label-title-sep))))

(define (pt-set-label-number-format gind) "1")
(define (pt-book-label-number-format gind) "1")
(define (pt-prefix-label-number-format gind) "1")
(define (pt-part-label-number-format gind) "I")
(define (pt-chapter-label-number-format gind) "1")
(define (pt-appendix-label-number-format gind) "A")
(define (pt-reference-label-number-format gind) "I")
(define (pt-example-label-number-format gind) "1")
(define (pt-figure-label-number-format gind) "1")
(define (pt-table-label-number-format gind) "1")
(define (pt-procedure-label-number-format gind) "1")
(define (pt-step-label-number-format gind) "1")
(define (pt-refsect1-label-number-format gind) "1")
(define (pt-refsect2-label-number-format gind) "1")
(define (pt-refsect3-label-number-format gind) "1")
(define (pt-sect1-label-number-format gind) "1")
(define (pt-sect2-label-number-format gind) "1")
(define (pt-sect3-label-number-format gind) "1")
(define (pt-sect4-label-number-format gind) "1")
(define (pt-sect5-label-number-format gind) "1")
(define (pt-default-label-number-format gind) "1")

(define (pt-label-number-format gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "set")) (pt-set-label-number-format gind))
     ((equal? name (normalize "book")) (pt-book-label-number-format gind))
     ((equal? name (normalize "prefix")) (pt-prefix-label-number-format gind))
     ((equal? name (normalize "part")) (pt-part-label-number-format gind))
     ((equal? name (normalize "chapter")) (pt-chapter-label-number-format gind))
     ((equal? name (normalize "appendix")) (pt-appendix-label-number-format gind))
     ((equal? name (normalize "reference")) (pt-reference-label-number-format gind))
     ((equal? name (normalize "example")) (pt-example-label-number-format gind))
     ((equal? name (normalize "figure")) (pt-figure-label-number-format gind))
     ((equal? name (normalize "table")) (pt-table-label-number-format gind))
     ((equal? name (normalize "procedure")) (pt-procedure-label-number-format gind))
     ((equal? name (normalize "step")) (pt-step-label-number-format gind))
     ((equal? name (normalize "refsect1")) (pt-refsect1-label-number-format gind))
     ((equal? name (normalize "refsect2")) (pt-refsect2-label-number-format gind))
     ((equal? name (normalize "refsect3")) (pt-refsect3-label-number-format gind))
     ((equal? name (normalize "sect1")) (pt-sect1-label-number-format gind))
     ((equal? name (normalize "sect2")) (pt-sect2-label-number-format gind))
     ((equal? name (normalize "sect3")) (pt-sect3-label-number-format gind))
     ((equal? name (normalize "sect4")) (pt-sect4-label-number-format gind))
     ((equal? name (normalize "sect5")) (pt-sect5-label-number-format gind))
     (else (pt-default-label-number-format gind)))))

(define ($lot-title-pt$ gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond ((equal? name (normalize "table"))    "&ListofTables;")
	  ((equal? name (normalize "example"))  "&ListofExamples;")
	  ((equal? name (normalize "figure"))   "&ListofFigures;")
	  ((equal? name (normalize "equation")) "&ListofEquations;")
	  (else (let* ((msg (string-append "&ListofUnknown;: " name))
		       (err (node-list-error msg (current-node))))
		  msg)))))

(define %gentext-pt-start-quote% (dingbat "ldquo"))

(define %gentext-pt-end-quote% (dingbat "rdquo"))

(define %gentext-pt-by% "&by;") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-pt-edited-by% "&Editedby;")
                           ;; "Edited by" Jane Doe

(define %gentext-pt-page% "")

(define %gentext-pt-and% "&and;")

(define %gentext-pt-bibl-pages% "&Pgs;")

(define %gentext-pt-endnotes% "&Notes;")

(define %gentext-pt-table-endnotes% "&TableNotes;:")

(define %gentext-pt-index-see% "&See;")

(define %gentext-pt-index-seealso% "&SeeAlso;")

