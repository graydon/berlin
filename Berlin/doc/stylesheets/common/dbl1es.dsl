;; $Id: dbl1es.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
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

(define (es-appendix-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Appendix; %n"
      "el &appendix; de nombre %t"))

(define (es-article-xref-string gi-or-name)
  (string-append %gentext-es-start-quote%
		 "%t"
		 %gentext-es-end-quote%))

(define (es-bibliography-xref-string gi-or-name)
  "%t")

(define (es-book-xref-string gi-or-name)
  "%t")

(define (es-chapter-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Chapter; %n"
      "el &chapter; de nombre %t"))

(define (es-equation-xref-string gi-or-name)
  "&Equation; %n")

(define (es-example-xref-string gi-or-name)
  "&Example; %n")

(define (es-figure-xref-string gi-or-name)
  "&Figure; %n")

(define (es-listitem-xref-string gi-or-name)
  "%n")

(define (es-part-xref-string gi-or-name)
  "&Part; %n")

(define (es-preface-xref-string gi-or-name)
  "%t")

(define (es-procedure-xref-string gi-or-name)
  "&Procedure; %n, %t")

(define (es-section-xref-string gi-or-name)
  (if %section-autolabel% 
      "&Section; %n" 
      "la &section; de nombre %t"))

(define (es-sect1-xref-string gi-or-name)
  (es-section-xref-string gi-or-name))

(define (es-sect2-xref-string gi-or-name)
  (es-section-xref-string gi-or-name))

(define (es-sect3-xref-string gi-or-name)
  (es-section-xref-string gi-or-name))

(define (es-sect4-xref-string gi-or-name)
  (es-section-xref-string gi-or-name))

(define (es-sect5-xref-string gi-or-name)
  (es-section-xref-string gi-or-name))

(define (es-step-xref-string gi-or-name)
  "&step; %n")

(define (es-table-xref-string gi-or-name)
  "&Table; %n")

(define (es-default-xref-string gi-or-name)
  (let* ((giname (if (string? gi-or-name) gi-or-name (gi gi-or-name)))
	 (msg    (string-append "[&xrefto; "
				(if giname giname "&nonexistantelement;")
				" &unsupported;]"))
	 (err    (node-list-error msg (current-node))))
    msg))

(define (gentext-es-xref-strings gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
      ((equal? name (normalize "appendix")) (es-appendix-xref-string gind))
      ((equal? name (normalize "article"))  (es-article-xref-string gind))
      ((equal? name (normalize "bibliography")) (es-bibliography-xref-string gind))
      ((equal? name (normalize "book"))     (es-book-xref-string gind))
      ((equal? name (normalize "chapter"))  (es-chapter-xref-string gind))
      ((equal? name (normalize "equation")) (es-equation-xref-string gind))
      ((equal? name (normalize "example"))  (es-example-xref-string gind))
      ((equal? name (normalize "figure"))   (es-figure-xref-string gind))
      ((equal? name (normalize "listitem")) (es-listitem-xref-string gind))
      ((equal? name (normalize "part"))     (es-part-xref-string gind))
      ((equal? name (normalize "preface"))  (es-preface-xref-string gind))
      ((equal? name (normalize "procedure")) (es-procedure-xref-string gind))
      ((equal? name (normalize "sect1"))    (es-sect1-xref-string gind))
      ((equal? name (normalize "sect2"))    (es-sect2-xref-string gind))
      ((equal? name (normalize "sect3"))    (es-sect3-xref-string gind))
      ((equal? name (normalize "sect4"))    (es-sect4-xref-string gind))
      ((equal? name (normalize "sect5"))    (es-sect5-xref-string gind))
      ((equal? name (normalize "step"))     (es-step-xref-string gind))
      ((equal? name (normalize "table"))    (es-table-xref-string gind))
      (else (es-default-xref-string gind)))))

(define (es-auto-xref-indirect-connector before) 
  (literal " &in; "))

;; Should the TOC come first or last?
;;
(define %generate-es-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define es-abstract-name	"&Abstract;")
(define es-appendix-name	"&Appendix;")
(define es-article-name	"&Article;")
(define es-bibliography-name	"&Bibliography;")
(define es-book-name		"&Book;")
(define es-calloutlist-name	"")
(define es-caution-name	"&Caution;")
(define es-chapter-name	"&Chapter;")
(define es-copyright-name	"&Copyright;")
(define es-dedication-name	"&Dedication;")
(define es-edition-name	"&Edition;")
(define es-equation-name	"&Equation;")
(define es-example-name	"&Example;")
(define es-figure-name	"&Figure;")
(define es-glossary-name	"&Glossary;")
(define es-glosssee-name	"&GlossSee;")
(define es-glossseealso-name	"&GlossSeeAlso;")
(define es-important-name	"&Important;")
(define es-index-name		"&Index;")
(define es-setindex-name	"&SetIndex;")
(define es-isbn-name		"&ISBN;")
(define es-legalnotice-name	"&LegalNotice;")
(define es-msgaud-name	"&MsgAud;")
(define es-msglevel-name	"&MsgLevel;")
(define es-msgorig-name	"&MsgOrig;")
(define es-note-name		"&Note;")
(define es-part-name		"&Part;")
(define es-preface-name	"&Preface;")
(define es-procedure-name	"&Procedure;")
(define es-pubdate-name	"&Published;")
(define es-reference-name	"&Reference;")
(define es-refname-name	"&RefName;")
(define es-revhistory-name	"&RevHistory;")
(define es-revision-name	"&Revision;")
(define es-sect1-name		"&Section;")
(define es-sect2-name		"&Section;")
(define es-sect3-name		"&Section;")
(define es-sect4-name		"&Section;")
(define es-sect5-name		"&Section;")
(define es-simplesect-name	"&Section;")
(define es-seeie-name		"&See;")
(define es-seealsoie-name	"&Seealso;")
(define es-set-name		"&Set;")
(define es-sidebar-name	"&Sidebar;")
(define es-step-name		"&step;")
(define es-table-name		"&Table;")
(define es-tip-name		"&Tip;")
(define es-toc-name		"&TableofContents;")
(define es-warning-name	"&Warning;")

(define (gentext-es-element-name gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract"))	es-abstract-name)
     ((equal? name (normalize "appendix"))	es-appendix-name)
     ((equal? name (normalize "article"))	es-article-name)
     ((equal? name (normalize "bibliography"))	es-bibliography-name)
     ((equal? name (normalize "book"))		es-book-name)
     ((equal? name (normalize "calloutlist"))	es-calloutlist-name)
     ((equal? name (normalize "caution"))	es-caution-name)
     ((equal? name (normalize "chapter"))	es-chapter-name)
     ((equal? name (normalize "copyright"))	es-copyright-name)
     ((equal? name (normalize "dedication"))	es-dedication-name)
     ((equal? name (normalize "edition"))	es-edition-name)
     ((equal? name (normalize "equation"))	es-equation-name)
     ((equal? name (normalize "example"))	es-example-name)
     ((equal? name (normalize "figure"))	es-figure-name)
     ((equal? name (normalize "glossary"))	es-glossary-name)
     ((equal? name (normalize "glosssee"))	es-glosssee-name)
     ((equal? name (normalize "glossseealso"))	es-glossseealso-name)
     ((equal? name (normalize "important"))	es-important-name)
     ((equal? name (normalize "index"))		es-index-name)
     ((equal? name (normalize "setindex"))	es-setindex-name)
     ((equal? name (normalize "isbn"))		es-isbn-name)
     ((equal? name (normalize "legalnotice"))	es-legalnotice-name)
     ((equal? name (normalize "msgaud"))	es-msgaud-name)
     ((equal? name (normalize "msglevel"))	es-msglevel-name)
     ((equal? name (normalize "msgorig"))	es-msgorig-name)
     ((equal? name (normalize "note"))		es-note-name)
     ((equal? name (normalize "part"))		es-part-name)
     ((equal? name (normalize "preface"))	es-preface-name)
     ((equal? name (normalize "procedure"))	es-procedure-name)
     ((equal? name (normalize "pubdate"))	es-pubdate-name)
     ((equal? name (normalize "reference"))	es-reference-name)
     ((equal? name (normalize "refname"))	es-refname-name)
     ((equal? name (normalize "revhistory"))	es-revhistory-name)
     ((equal? name (normalize "revision"))	es-revision-name)
     ((equal? name (normalize "sect1"))		es-sect1-name)
     ((equal? name (normalize "sect2"))		es-sect2-name)
     ((equal? name (normalize "sect3"))		es-sect3-name)
     ((equal? name (normalize "sect4"))		es-sect4-name)
     ((equal? name (normalize "sect5"))		es-sect5-name)
     ((equal? name (normalize "simplesect"))    es-simplesect-name)
     ((equal? name (normalize "seeie"))		es-seeie-name)
     ((equal? name (normalize "seealsoie"))	es-seealsoie-name)
     ((equal? name (normalize "set"))		es-set-name)
     ((equal? name (normalize "sidebar"))	es-sidebar-name)
     ((equal? name (normalize "step"))		es-step-name)
     ((equal? name (normalize "table"))		es-table-name)
     ((equal? name (normalize "tip"))		es-tip-name)
     ((equal? name (normalize "toc"))		es-toc-name)
     ((equal? name (normalize "warning"))	es-warning-name)
     (else (let* ((msg (string-append "&unexpectedelementname;: " name))
		  (err (node-list-error msg (current-node))))
	     msg)))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-es-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;

(define es-equation-intra-label-sep "-")
(define es-example-intra-label-sep "-")
(define es-figure-intra-label-sep "-")
(define es-procedure-intra-label-sep ".")
(define es-refentry-intra-label-sep ".")
(define es-reference-intra-label-sep ".")
(define es-refname-intra-label-sep ", ")
(define es-refsect1-intra-label-sep ".")
(define es-refsect2-intra-label-sep ".")
(define es-refsect3-intra-label-sep ".")
(define es-sect1-intra-label-sep ".")
(define es-sect2-intra-label-sep ".")
(define es-sect3-intra-label-sep ".")
(define es-sect4-intra-label-sep ".")
(define es-sect5-intra-label-sep ".")
(define es-step-intra-label-sep ".")
(define es-table-intra-label-sep "-")
(define es-_pagenumber-intra-label-sep "-")
(define es-default-intra-label-sep "")

(define (gentext-es-intra-label-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "equation"))	es-equation-intra-label-sep)
     ((equal? name (normalize "example"))	es-example-intra-label-sep)
     ((equal? name (normalize "figure"))	es-figure-intra-label-sep)
     ((equal? name (normalize "procedure"))	es-procedure-intra-label-sep)
     ((equal? name (normalize "refentry"))	es-refentry-intra-label-sep)
     ((equal? name (normalize "reference"))	es-reference-intra-label-sep)
     ((equal? name (normalize "refname"))	es-refname-intra-label-sep)
     ((equal? name (normalize "refsect1"))	es-refsect1-intra-label-sep)
     ((equal? name (normalize "refsect2"))	es-refsect2-intra-label-sep)
     ((equal? name (normalize "refsect3"))	es-refsect3-intra-label-sep)
     ((equal? name (normalize "sect1"))		es-sect1-intra-label-sep)
     ((equal? name (normalize "sect2"))		es-sect2-intra-label-sep)
     ((equal? name (normalize "sect3"))		es-sect3-intra-label-sep)
     ((equal? name (normalize "sect4"))		es-sect4-intra-label-sep)
     ((equal? name (normalize "sect5"))		es-sect5-intra-label-sep)
     ((equal? name (normalize "step"))		es-step-intra-label-sep)
     ((equal? name (normalize "table"))		es-table-intra-label-sep)
     ((equal? name (normalize "_pagenumber"))	es-_pagenumber-intra-label-sep)
     (else es-default-intra-label-sep))))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;

(define es-abstract-label-title-sep ": ")
(define es-appendix-label-title-sep ". ")
(define es-caution-label-title-sep "")
(define es-chapter-label-title-sep ". ")
(define es-equation-label-title-sep ". ")
(define es-example-label-title-sep ". ")
(define es-figure-label-title-sep ". ")
(define es-footnote-label-title-sep ". ")
(define es-glosssee-label-title-sep ": ")
(define es-glossseealso-label-title-sep ": ")
(define es-important-label-title-sep ": ")
(define es-note-label-title-sep ": ")
(define es-orderedlist-label-title-sep ". ")
(define es-part-label-title-sep ". ")
(define es-procedure-label-title-sep ". ")
(define es-prefix-label-title-sep ". ")
(define es-refentry-label-title-sep "")
(define es-reference-label-title-sep ". ")
(define es-refsect1-label-title-sep ". ")
(define es-refsect2-label-title-sep ". ")
(define es-refsect3-label-title-sep ". ")
(define es-sect1-label-title-sep ". ")
(define es-sect2-label-title-sep ". ")
(define es-sect3-label-title-sep ". ")
(define es-sect4-label-title-sep ". ")
(define es-sect5-label-title-sep ". ")
(define es-seeie-label-title-sep " ")
(define es-seealsoie-label-title-sep " ")
(define es-step-label-title-sep ". ")
(define es-table-label-title-sep ". ")
(define es-tip-label-title-sep ": ")
(define es-warning-label-title-sep "")
(define es-default-label-title-sep "")

(define (gentext-es-label-title-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract")) es-abstract-label-title-sep)
     ((equal? name (normalize "appendix")) es-appendix-label-title-sep)
     ((equal? name (normalize "caution")) es-caution-label-title-sep)
     ((equal? name (normalize "chapter")) es-chapter-label-title-sep)
     ((equal? name (normalize "equation")) es-equation-label-title-sep)
     ((equal? name (normalize "example")) es-example-label-title-sep)
     ((equal? name (normalize "figure")) es-figure-label-title-sep)
     ((equal? name (normalize "footnote")) es-footnote-label-title-sep)
     ((equal? name (normalize "glosssee")) es-glosssee-label-title-sep)
     ((equal? name (normalize "glossseealso")) es-glossseealso-label-title-sep)
     ((equal? name (normalize "important")) es-important-label-title-sep)
     ((equal? name (normalize "note")) es-note-label-title-sep)
     ((equal? name (normalize "orderedlist")) es-orderedlist-label-title-sep)
     ((equal? name (normalize "part")) es-part-label-title-sep)
     ((equal? name (normalize "procedure")) es-procedure-label-title-sep)
     ((equal? name (normalize "prefix")) es-prefix-label-title-sep)
     ((equal? name (normalize "refentry")) es-refentry-label-title-sep)
     ((equal? name (normalize "reference")) es-reference-label-title-sep)
     ((equal? name (normalize "refsect1")) es-refsect1-label-title-sep)
     ((equal? name (normalize "refsect2")) es-refsect2-label-title-sep)
     ((equal? name (normalize "refsect3")) es-refsect3-label-title-sep)
     ((equal? name (normalize "sect1")) es-sect1-label-title-sep)
     ((equal? name (normalize "sect2")) es-sect2-label-title-sep)
     ((equal? name (normalize "sect3")) es-sect3-label-title-sep)
     ((equal? name (normalize "sect4")) es-sect4-label-title-sep)
     ((equal? name (normalize "sect5")) es-sect5-label-title-sep)
     ((equal? name (normalize "seeie")) es-seeie-label-title-sep)
     ((equal? name (normalize "seealsoie")) es-seealsoie-label-title-sep)
     ((equal? name (normalize "step")) es-step-label-title-sep)
     ((equal? name (normalize "table")) es-table-label-title-sep)
     ((equal? name (normalize "tip")) es-tip-label-title-sep)
     ((equal? name (normalize "warning")) es-warning-label-title-sep)
     (else es-default-label-title-sep))))

(define (es-set-label-number-format gind) "1")
(define (es-book-label-number-format gind) "1")
(define (es-prefix-label-number-format gind) "1")
(define (es-part-label-number-format gind) "I")
(define (es-chapter-label-number-format gind) "1")
(define (es-appendix-label-number-format gind) "A")
(define (es-reference-label-number-format gind) "I")
(define (es-example-label-number-format gind) "1")
(define (es-figure-label-number-format gind) "1")
(define (es-table-label-number-format gind) "1")
(define (es-procedure-label-number-format gind) "1")
(define (es-step-label-number-format gind) "1")
(define (es-refsect1-label-number-format gind) "1")
(define (es-refsect2-label-number-format gind) "1")
(define (es-refsect3-label-number-format gind) "1")
(define (es-sect1-label-number-format gind) "1")
(define (es-sect2-label-number-format gind) "1")
(define (es-sect3-label-number-format gind) "1")
(define (es-sect4-label-number-format gind) "1")
(define (es-sect5-label-number-format gind) "1")
(define (es-default-label-number-format gind) "1")

(define (es-label-number-format gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "set")) (es-set-label-number-format gind))
     ((equal? name (normalize "book")) (es-book-label-number-format gind))
     ((equal? name (normalize "prefix")) (es-prefix-label-number-format gind))
     ((equal? name (normalize "part")) (es-part-label-number-format gind))
     ((equal? name (normalize "chapter")) (es-chapter-label-number-format gind))
     ((equal? name (normalize "appendix")) (es-appendix-label-number-format gind))
     ((equal? name (normalize "reference")) (es-reference-label-number-format gind))
     ((equal? name (normalize "example")) (es-example-label-number-format gind))
     ((equal? name (normalize "figure")) (es-figure-label-number-format gind))
     ((equal? name (normalize "table")) (es-table-label-number-format gind))
     ((equal? name (normalize "procedure")) (es-procedure-label-number-format gind))
     ((equal? name (normalize "step")) (es-step-label-number-format gind))
     ((equal? name (normalize "refsect1")) (es-refsect1-label-number-format gind))
     ((equal? name (normalize "refsect2")) (es-refsect2-label-number-format gind))
     ((equal? name (normalize "refsect3")) (es-refsect3-label-number-format gind))
     ((equal? name (normalize "sect1")) (es-sect1-label-number-format gind))
     ((equal? name (normalize "sect2")) (es-sect2-label-number-format gind))
     ((equal? name (normalize "sect3")) (es-sect3-label-number-format gind))
     ((equal? name (normalize "sect4")) (es-sect4-label-number-format gind))
     ((equal? name (normalize "sect5")) (es-sect5-label-number-format gind))
     (else (es-default-label-number-format gind)))))

(define ($lot-title-es$ gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond ((equal? name (normalize "table"))    "&ListofTables;")
	  ((equal? name (normalize "example"))  "&ListofExamples;")
	  ((equal? name (normalize "figure"))   "&ListofFigures;")
	  ((equal? name (normalize "equation")) "&ListofEquations;")
	  (else (let* ((msg (string-append "&ListofUnknown;: " name))
		       (err (node-list-error msg (current-node))))
		  msg)))))

(define %gentext-es-start-quote% (dingbat "ldquo"))

(define %gentext-es-end-quote% (dingbat "rdquo"))

(define %gentext-es-by% "&by;") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-es-edited-by% "&Editedby;")
                           ;; "Edited by" Jane Doe

(define %gentext-es-page% "")

(define %gentext-es-and% "&and;")

(define %gentext-es-bibl-pages% "&Pgs;")

(define %gentext-es-endnotes% "&Notes;")

(define %gentext-es-table-endnotes% "&TableNotes;:")

(define %gentext-es-index-see% "&See;")

(define %gentext-es-index-seealso% "&SeeAlso;")

