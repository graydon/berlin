;; $Id: dbl1nl.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
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

(define (nl-appendix-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Appendix; %n"
      "&appendix; %t"))

(define (nl-article-xref-string gi-or-name)
  (string-append %gentext-nl-start-quote%
		 "%t"
		 %gentext-nl-end-quote%))

(define (nl-bibliography-xref-string gi-or-name)
  "%t")

(define (nl-book-xref-string gi-or-name)
  "%t")

(define (nl-chapter-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Chapter; %n"
      "&chapter; %t"))

(define (nl-equation-xref-string gi-or-name)
  "&Equation; %n")

(define (nl-example-xref-string gi-or-name)
  "&Example; %n")

(define (nl-figure-xref-string gi-or-name)
  "&Figure; %n")

(define (nl-listitem-xref-string gi-or-name)
  "%n")

(define (nl-part-xref-string gi-or-name)
  "&Part; %n")

(define (nl-preface-xref-string gi-or-name)
  "%t")

(define (nl-procedure-xref-string gi-or-name)
  "&Procedure; %n, %t")

(define (nl-section-xref-string gi-or-name)
  (if %section-autolabel% 
      "&Section; %n" 
      "&section; %t"))

(define (nl-sect1-xref-string gi-or-name)
  (nl-section-xref-string gi-or-name))

(define (nl-sect2-xref-string gi-or-name)
  (nl-section-xref-string gi-or-name))

(define (nl-sect3-xref-string gi-or-name)
  (nl-section-xref-string gi-or-name))

(define (nl-sect4-xref-string gi-or-name)
  (nl-section-xref-string gi-or-name))

(define (nl-sect5-xref-string gi-or-name)
  (nl-section-xref-string gi-or-name))

(define (nl-step-xref-string gi-or-name)
  "&step; %n")

(define (nl-table-xref-string gi-or-name)
  "&Table; %n")

(define (nl-default-xref-string gi-or-name)
  (let* ((giname (if (string? gi-or-name) gi-or-name (gi gi-or-name)))
	 (msg    (string-append "[&xrefto; "
				(if giname giname "&nonexistantelement;")
				" &unsupported;]"))
	 (err    (node-list-error msg (current-node))))
    msg))

(define (gentext-nl-xref-strings gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
      ((equal? name (normalize "appendix")) (nl-appendix-xref-string gind))
      ((equal? name (normalize "article"))  (nl-article-xref-string gind))
      ((equal? name (normalize "bibliography")) (nl-bibliography-xref-string gind))
      ((equal? name (normalize "book"))     (nl-book-xref-string gind))
      ((equal? name (normalize "chapter"))  (nl-chapter-xref-string gind))
      ((equal? name (normalize "equation")) (nl-equation-xref-string gind))
      ((equal? name (normalize "example"))  (nl-example-xref-string gind))
      ((equal? name (normalize "figure"))   (nl-figure-xref-string gind))
      ((equal? name (normalize "listitem")) (nl-listitem-xref-string gind))
      ((equal? name (normalize "part"))     (nl-part-xref-string gind))
      ((equal? name (normalize "preface"))  (nl-preface-xref-string gind))
      ((equal? name (normalize "procedure")) (nl-procedure-xref-string gind))
      ((equal? name (normalize "sect1"))    (nl-sect1-xref-string gind))
      ((equal? name (normalize "sect2"))    (nl-sect2-xref-string gind))
      ((equal? name (normalize "sect3"))    (nl-sect3-xref-string gind))
      ((equal? name (normalize "sect4"))    (nl-sect4-xref-string gind))
      ((equal? name (normalize "sect5"))    (nl-sect5-xref-string gind))
      ((equal? name (normalize "step"))     (nl-step-xref-string gind))
      ((equal? name (normalize "table"))    (nl-table-xref-string gind))
      (else (nl-default-xref-string gind)))))

(define (nl-auto-xref-indirect-connector before) 
  (literal " &in; "))

;; Should the TOC come first or last?
;;
(define %generate-nl-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define nl-abstract-name	"&Abstract;")
(define nl-appendix-name	"&Appendix;")
(define nl-article-name	"&Article;")
(define nl-bibliography-name	"&Bibliography;")
(define nl-book-name		"&Book;")
(define nl-calloutlist-name	"")
(define nl-caution-name	"&Caution;")
(define nl-chapter-name	"&Chapter;")
(define nl-copyright-name	"&Copyright;")
(define nl-dedication-name	"&Dedication;")
(define nl-edition-name	"&Edition;")
(define nl-equation-name	"&Equation;")
(define nl-example-name	"&Example;")
(define nl-figure-name	"&Figure;")
(define nl-glossary-name	"&Glossary;")
(define nl-glosssee-name	"&GlossSee;")
(define nl-glossseealso-name	"&GlossSeeAlso;")
(define nl-important-name	"&Important;")
(define nl-index-name		"&Index;")
(define nl-setindex-name	"&SetIndex;")
(define nl-isbn-name		"&ISBN;")
(define nl-legalnotice-name	"&LegalNotice;")
(define nl-msgaud-name	"&MsgAud;")
(define nl-msglevel-name	"&MsgLevel;")
(define nl-msgorig-name	"&MsgOrig;")
(define nl-note-name		"&Note;")
(define nl-part-name		"&Part;")
(define nl-preface-name	"&Preface;")
(define nl-procedure-name	"&Procedure;")
(define nl-pubdate-name	"&Published;")
(define nl-reference-name	"&Reference;")
(define nl-refname-name	"&RefName;")
(define nl-revhistory-name	"&RevHistory;")
(define nl-revision-name	"&Revision;")
(define nl-sect1-name		"&Section;")
(define nl-sect2-name		"&Section;")
(define nl-sect3-name		"&Section;")
(define nl-sect4-name		"&Section;")
(define nl-sect5-name		"&Section;")
(define nl-simplesect-name	"&Section;")
(define nl-seeie-name		"&See;")
(define nl-seealsoie-name	"&Seealso;")
(define nl-set-name		"&Set;")
(define nl-sidebar-name	"&Sidebar;")
(define nl-step-name		"&step;")
(define nl-table-name		"&Table;")
(define nl-tip-name		"&Tip;")
(define nl-toc-name		"&TableofContents;")
(define nl-warning-name	"&Warning;")

(define (gentext-nl-element-name gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract"))	nl-abstract-name)
     ((equal? name (normalize "appendix"))	nl-appendix-name)
     ((equal? name (normalize "article"))	nl-article-name)
     ((equal? name (normalize "bibliography"))	nl-bibliography-name)
     ((equal? name (normalize "book"))		nl-book-name)
     ((equal? name (normalize "calloutlist"))	nl-calloutlist-name)
     ((equal? name (normalize "caution"))	nl-caution-name)
     ((equal? name (normalize "chapter"))	nl-chapter-name)
     ((equal? name (normalize "copyright"))	nl-copyright-name)
     ((equal? name (normalize "dedication"))	nl-dedication-name)
     ((equal? name (normalize "edition"))	nl-edition-name)
     ((equal? name (normalize "equation"))	nl-equation-name)
     ((equal? name (normalize "example"))	nl-example-name)
     ((equal? name (normalize "figure"))	nl-figure-name)
     ((equal? name (normalize "glossary"))	nl-glossary-name)
     ((equal? name (normalize "glosssee"))	nl-glosssee-name)
     ((equal? name (normalize "glossseealso"))	nl-glossseealso-name)
     ((equal? name (normalize "important"))	nl-important-name)
     ((equal? name (normalize "index"))		nl-index-name)
     ((equal? name (normalize "setindex"))	nl-setindex-name)
     ((equal? name (normalize "isbn"))		nl-isbn-name)
     ((equal? name (normalize "legalnotice"))	nl-legalnotice-name)
     ((equal? name (normalize "msgaud"))	nl-msgaud-name)
     ((equal? name (normalize "msglevel"))	nl-msglevel-name)
     ((equal? name (normalize "msgorig"))	nl-msgorig-name)
     ((equal? name (normalize "note"))		nl-note-name)
     ((equal? name (normalize "part"))		nl-part-name)
     ((equal? name (normalize "preface"))	nl-preface-name)
     ((equal? name (normalize "procedure"))	nl-procedure-name)
     ((equal? name (normalize "pubdate"))	nl-pubdate-name)
     ((equal? name (normalize "reference"))	nl-reference-name)
     ((equal? name (normalize "refname"))	nl-refname-name)
     ((equal? name (normalize "revhistory"))	nl-revhistory-name)
     ((equal? name (normalize "revision"))	nl-revision-name)
     ((equal? name (normalize "sect1"))		nl-sect1-name)
     ((equal? name (normalize "sect2"))		nl-sect2-name)
     ((equal? name (normalize "sect3"))		nl-sect3-name)
     ((equal? name (normalize "sect4"))		nl-sect4-name)
     ((equal? name (normalize "sect5"))		nl-sect5-name)
     ((equal? name (normalize "simplesect"))    nl-simplesect-name)
     ((equal? name (normalize "seeie"))		nl-seeie-name)
     ((equal? name (normalize "seealsoie"))	nl-seealsoie-name)
     ((equal? name (normalize "set"))		nl-set-name)
     ((equal? name (normalize "sidebar"))	nl-sidebar-name)
     ((equal? name (normalize "step"))		nl-step-name)
     ((equal? name (normalize "table"))		nl-table-name)
     ((equal? name (normalize "tip"))		nl-tip-name)
     ((equal? name (normalize "toc"))		nl-toc-name)
     ((equal? name (normalize "warning"))	nl-warning-name)
     (else (let* ((msg (string-append "&unexpectedelementname;: " name))
		  (err (node-list-error msg (current-node))))
	     msg)))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-nl-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;

(define nl-equation-intra-label-sep "-")
(define nl-example-intra-label-sep "-")
(define nl-figure-intra-label-sep "-")
(define nl-procedure-intra-label-sep ".")
(define nl-refentry-intra-label-sep ".")
(define nl-reference-intra-label-sep ".")
(define nl-refname-intra-label-sep ", ")
(define nl-refsect1-intra-label-sep ".")
(define nl-refsect2-intra-label-sep ".")
(define nl-refsect3-intra-label-sep ".")
(define nl-sect1-intra-label-sep ".")
(define nl-sect2-intra-label-sep ".")
(define nl-sect3-intra-label-sep ".")
(define nl-sect4-intra-label-sep ".")
(define nl-sect5-intra-label-sep ".")
(define nl-step-intra-label-sep ".")
(define nl-table-intra-label-sep "-")
(define nl-_pagenumber-intra-label-sep "-")
(define nl-default-intra-label-sep "")

(define (gentext-nl-intra-label-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "equation"))	nl-equation-intra-label-sep)
     ((equal? name (normalize "example"))	nl-example-intra-label-sep)
     ((equal? name (normalize "figure"))	nl-figure-intra-label-sep)
     ((equal? name (normalize "procedure"))	nl-procedure-intra-label-sep)
     ((equal? name (normalize "refentry"))	nl-refentry-intra-label-sep)
     ((equal? name (normalize "reference"))	nl-reference-intra-label-sep)
     ((equal? name (normalize "refname"))	nl-refname-intra-label-sep)
     ((equal? name (normalize "refsect1"))	nl-refsect1-intra-label-sep)
     ((equal? name (normalize "refsect2"))	nl-refsect2-intra-label-sep)
     ((equal? name (normalize "refsect3"))	nl-refsect3-intra-label-sep)
     ((equal? name (normalize "sect1"))		nl-sect1-intra-label-sep)
     ((equal? name (normalize "sect2"))		nl-sect2-intra-label-sep)
     ((equal? name (normalize "sect3"))		nl-sect3-intra-label-sep)
     ((equal? name (normalize "sect4"))		nl-sect4-intra-label-sep)
     ((equal? name (normalize "sect5"))		nl-sect5-intra-label-sep)
     ((equal? name (normalize "step"))		nl-step-intra-label-sep)
     ((equal? name (normalize "table"))		nl-table-intra-label-sep)
     ((equal? name (normalize "_pagenumber"))	nl-_pagenumber-intra-label-sep)
     (else nl-default-intra-label-sep))))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;

(define nl-abstract-label-title-sep ": ")
(define nl-appendix-label-title-sep ". ")
(define nl-caution-label-title-sep "")
(define nl-chapter-label-title-sep ". ")
(define nl-equation-label-title-sep ". ")
(define nl-example-label-title-sep ". ")
(define nl-figure-label-title-sep ". ")
(define nl-footnote-label-title-sep ". ")
(define nl-glosssee-label-title-sep ": ")
(define nl-glossseealso-label-title-sep ": ")
(define nl-important-label-title-sep ": ")
(define nl-note-label-title-sep ": ")
(define nl-orderedlist-label-title-sep ". ")
(define nl-part-label-title-sep ". ")
(define nl-procedure-label-title-sep ". ")
(define nl-prefix-label-title-sep ". ")
(define nl-refentry-label-title-sep "")
(define nl-reference-label-title-sep ". ")
(define nl-refsect1-label-title-sep ". ")
(define nl-refsect2-label-title-sep ". ")
(define nl-refsect3-label-title-sep ". ")
(define nl-sect1-label-title-sep ". ")
(define nl-sect2-label-title-sep ". ")
(define nl-sect3-label-title-sep ". ")
(define nl-sect4-label-title-sep ". ")
(define nl-sect5-label-title-sep ". ")
(define nl-seeie-label-title-sep " ")
(define nl-seealsoie-label-title-sep " ")
(define nl-step-label-title-sep ". ")
(define nl-table-label-title-sep ". ")
(define nl-tip-label-title-sep ": ")
(define nl-warning-label-title-sep "")
(define nl-default-label-title-sep "")

(define (gentext-nl-label-title-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract")) nl-abstract-label-title-sep)
     ((equal? name (normalize "appendix")) nl-appendix-label-title-sep)
     ((equal? name (normalize "caution")) nl-caution-label-title-sep)
     ((equal? name (normalize "chapter")) nl-chapter-label-title-sep)
     ((equal? name (normalize "equation")) nl-equation-label-title-sep)
     ((equal? name (normalize "example")) nl-example-label-title-sep)
     ((equal? name (normalize "figure")) nl-figure-label-title-sep)
     ((equal? name (normalize "footnote")) nl-footnote-label-title-sep)
     ((equal? name (normalize "glosssee")) nl-glosssee-label-title-sep)
     ((equal? name (normalize "glossseealso")) nl-glossseealso-label-title-sep)
     ((equal? name (normalize "important")) nl-important-label-title-sep)
     ((equal? name (normalize "note")) nl-note-label-title-sep)
     ((equal? name (normalize "orderedlist")) nl-orderedlist-label-title-sep)
     ((equal? name (normalize "part")) nl-part-label-title-sep)
     ((equal? name (normalize "procedure")) nl-procedure-label-title-sep)
     ((equal? name (normalize "prefix")) nl-prefix-label-title-sep)
     ((equal? name (normalize "refentry")) nl-refentry-label-title-sep)
     ((equal? name (normalize "reference")) nl-reference-label-title-sep)
     ((equal? name (normalize "refsect1")) nl-refsect1-label-title-sep)
     ((equal? name (normalize "refsect2")) nl-refsect2-label-title-sep)
     ((equal? name (normalize "refsect3")) nl-refsect3-label-title-sep)
     ((equal? name (normalize "sect1")) nl-sect1-label-title-sep)
     ((equal? name (normalize "sect2")) nl-sect2-label-title-sep)
     ((equal? name (normalize "sect3")) nl-sect3-label-title-sep)
     ((equal? name (normalize "sect4")) nl-sect4-label-title-sep)
     ((equal? name (normalize "sect5")) nl-sect5-label-title-sep)
     ((equal? name (normalize "seeie")) nl-seeie-label-title-sep)
     ((equal? name (normalize "seealsoie")) nl-seealsoie-label-title-sep)
     ((equal? name (normalize "step")) nl-step-label-title-sep)
     ((equal? name (normalize "table")) nl-table-label-title-sep)
     ((equal? name (normalize "tip")) nl-tip-label-title-sep)
     ((equal? name (normalize "warning")) nl-warning-label-title-sep)
     (else nl-default-label-title-sep))))

(define (nl-set-label-number-format gind) "1")
(define (nl-book-label-number-format gind) "1")
(define (nl-prefix-label-number-format gind) "1")
(define (nl-part-label-number-format gind) "I")
(define (nl-chapter-label-number-format gind) "1")
(define (nl-appendix-label-number-format gind) "A")
(define (nl-reference-label-number-format gind) "I")
(define (nl-example-label-number-format gind) "1")
(define (nl-figure-label-number-format gind) "1")
(define (nl-table-label-number-format gind) "1")
(define (nl-procedure-label-number-format gind) "1")
(define (nl-step-label-number-format gind) "1")
(define (nl-refsect1-label-number-format gind) "1")
(define (nl-refsect2-label-number-format gind) "1")
(define (nl-refsect3-label-number-format gind) "1")
(define (nl-sect1-label-number-format gind) "1")
(define (nl-sect2-label-number-format gind) "1")
(define (nl-sect3-label-number-format gind) "1")
(define (nl-sect4-label-number-format gind) "1")
(define (nl-sect5-label-number-format gind) "1")
(define (nl-default-label-number-format gind) "1")

(define (nl-label-number-format gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "set")) (nl-set-label-number-format gind))
     ((equal? name (normalize "book")) (nl-book-label-number-format gind))
     ((equal? name (normalize "prefix")) (nl-prefix-label-number-format gind))
     ((equal? name (normalize "part")) (nl-part-label-number-format gind))
     ((equal? name (normalize "chapter")) (nl-chapter-label-number-format gind))
     ((equal? name (normalize "appendix")) (nl-appendix-label-number-format gind))
     ((equal? name (normalize "reference")) (nl-reference-label-number-format gind))
     ((equal? name (normalize "example")) (nl-example-label-number-format gind))
     ((equal? name (normalize "figure")) (nl-figure-label-number-format gind))
     ((equal? name (normalize "table")) (nl-table-label-number-format gind))
     ((equal? name (normalize "procedure")) (nl-procedure-label-number-format gind))
     ((equal? name (normalize "step")) (nl-step-label-number-format gind))
     ((equal? name (normalize "refsect1")) (nl-refsect1-label-number-format gind))
     ((equal? name (normalize "refsect2")) (nl-refsect2-label-number-format gind))
     ((equal? name (normalize "refsect3")) (nl-refsect3-label-number-format gind))
     ((equal? name (normalize "sect1")) (nl-sect1-label-number-format gind))
     ((equal? name (normalize "sect2")) (nl-sect2-label-number-format gind))
     ((equal? name (normalize "sect3")) (nl-sect3-label-number-format gind))
     ((equal? name (normalize "sect4")) (nl-sect4-label-number-format gind))
     ((equal? name (normalize "sect5")) (nl-sect5-label-number-format gind))
     (else (nl-default-label-number-format gind)))))

(define ($lot-title-nl$ gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond ((equal? name (normalize "table"))    "&ListofTables;")
	  ((equal? name (normalize "example"))  "&ListofExamples;")
	  ((equal? name (normalize "figure"))   "&ListofFigures;")
	  ((equal? name (normalize "equation")) "&ListofEquations;")
	  (else (let* ((msg (string-append "&ListofUnknown;: " name))
		       (err (node-list-error msg (current-node))))
		  msg)))))

(define %gentext-nl-start-quote% (dingbat "ldquo"))

(define %gentext-nl-end-quote% (dingbat "rdquo"))

(define %gentext-nl-by% "&by;") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-nl-edited-by% "&Editedby;")
                           ;; "Edited by" Jane Doe

(define %gentext-nl-page% "")

(define %gentext-nl-and% "&and;")

(define %gentext-nl-bibl-pages% "&Pgs;")

(define %gentext-nl-endnotes% "&Notes;")

(define %gentext-nl-table-endnotes% "&TableNotes;:")

(define %gentext-nl-index-see% "&See;")

(define %gentext-nl-index-seealso% "&SeeAlso;")

