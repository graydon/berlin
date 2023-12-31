;; $Id: dbl1ru.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; ----------------------------- Localization -----------------------------

;; If you create a new version of this file, please send it to
;; Norman Walsh, ndw@nwalsh.com

;; This is revised Russian Localization version by Norman Walsh, modified by
;; Ilia V. Kouznetsov, ilia@syntext.com

;; Suggestion of I. Kouznetsov: According to my experience of writing
;; documentation, Russian Words for cross references had better be
;; abbreviated (just like it is usually done in Russian documents)
;; because the ends of not abbreviated cross references may vary from
;; place to place due to the cases of Russian language. Due to this reason
;; entities with ".abr" syffix are added.

;; The generated text for cross references to elements.  See dblink.dsl
;; for a discussion of how substitution is performed on the %x
;; keywords.
;;

(define (ru-appendix-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Appendix.abr; %n"
      "&Appendix.abr; %t"))

(define (ru-article-xref-string gi-or-name)
  (string-append %gentext-ru-start-quote%
		 "%t"
		 %gentext-ru-end-quote%))

(define (ru-bibliography-xref-string gi-or-name)
  "%t")

(define (ru-book-xref-string gi-or-name)
  "%t")

(define (ru-chapter-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Chapter.abr; %n"
      "&Chapter.abr; %t"))

(define (ru-equation-xref-string gi-or-name)
  "&Equation.abr; %n")

(define (ru-example-xref-string gi-or-name)
  "&Example.abr; %n")

(define (ru-figure-xref-string gi-or-name)
  "&Figure.abr; %n")

(define (ru-listitem-xref-string gi-or-name)
  "%n")

(define (ru-part-xref-string gi-or-name)
  "&Part.abr; %n")

(define (ru-preface-xref-string gi-or-name)
  "%t")

(define (ru-procedure-xref-string gi-or-name)
  "&Procedure.abr; %n, %t")

(define (ru-section-xref-string gi-or-name)
  (if %section-autolabel% 
      "&Section.abr; %n" 
      "&Section.abr; %t"))

(define (ru-sect1-xref-string gi-or-name)
  (ru-section-xref-string gi-or-name))

(define (ru-sect2-xref-string gi-or-name)
  (ru-section-xref-string gi-or-name))

(define (ru-sect3-xref-string gi-or-name)
  (ru-section-xref-string gi-or-name))

(define (ru-sect4-xref-string gi-or-name)
  (ru-section-xref-string gi-or-name))

(define (ru-sect5-xref-string gi-or-name)
  (ru-section-xref-string gi-or-name))

(define (ru-step-xref-string gi-or-name)
  "&step; %n")

(define (ru-table-xref-string gi-or-name)
  "&Table.abr; %n")

(define (ru-default-xref-string gi-or-name)
  (let* ((giname (if (string? gi-or-name) gi-or-name (gi gi-or-name)))
	 (msg    (string-append "[&xrefto; "
				(if giname giname "&nonexistantelement;")
				" &unsupported;]"))
	 (err    (node-list-error msg (current-node))))
    msg))

(define (gentext-ru-xref-strings gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
      ((equal? name (normalize "appendix")) (ru-appendix-xref-string gind))
      ((equal? name (normalize "article"))  (ru-article-xref-string gind))
      ((equal? name (normalize "bibliography")) (ru-bibliography-xref-string gind))
      ((equal? name (normalize "book"))     (ru-book-xref-string gind))
      ((equal? name (normalize "chapter"))  (ru-chapter-xref-string gind))
      ((equal? name (normalize "equation")) (ru-equation-xref-string gind))
      ((equal? name (normalize "example"))  (ru-example-xref-string gind))
      ((equal? name (normalize "figure"))   (ru-figure-xref-string gind))
      ((equal? name (normalize "listitem")) (ru-listitem-xref-string gind))
      ((equal? name (normalize "part"))     (ru-part-xref-string gind))
      ((equal? name (normalize "preface"))  (ru-preface-xref-string gind))
      ((equal? name (normalize "procedure")) (ru-procedure-xref-string gind))
      ((equal? name (normalize "sect1"))    (ru-sect1-xref-string gind))
      ((equal? name (normalize "sect2"))    (ru-sect2-xref-string gind))
      ((equal? name (normalize "sect3"))    (ru-sect3-xref-string gind))
      ((equal? name (normalize "sect4"))    (ru-sect4-xref-string gind))
      ((equal? name (normalize "sect5"))    (ru-sect5-xref-string gind))
      ((equal? name (normalize "step"))     (ru-step-xref-string gind))
      ((equal? name (normalize "table"))    (ru-table-xref-string gind))
      (else (ru-default-xref-string gind)))))

(define (ru-auto-xref-indirect-connector before) 
  (literal " &in; "))

;; Should the TOC come first or last?
;;
(define %generate-ru-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define ru-abstract-name	"&Abstract;")
(define ru-appendix-name	"&Appendix;")
(define ru-article-name	"&Article;")
(define ru-bibliography-name	"&Bibliography;")
(define ru-book-name		"&Book;")
(define ru-calloutlist-name	"")
(define ru-caution-name	"&Caution;")
(define ru-chapter-name	"&Chapter;")
(define ru-copyright-name	"&Copyright;")
(define ru-dedication-name	"&Dedication;")
(define ru-edition-name	"&Edition;")
(define ru-equation-name	"&Equation;")
(define ru-example-name	"&Example;")
(define ru-figure-name	"&Figure;")
(define ru-glossary-name	"&Glossary;")
(define ru-glosssee-name	"&See;")
(define ru-glossseealso-name	"&SeeAlso;")
(define ru-important-name	"&Important;")
(define ru-index-name		"&Index;")
(define ru-setindex-name	"&SetIndex;")
(define ru-isbn-name		"&ISBN;")
(define ru-legalnotice-name	"")
(define ru-msgaud-name	"&Audience;")
(define ru-msglevel-name	"&Level;")
(define ru-msgorig-name	"&Origin;")
(define ru-note-name		"&Note;")
(define ru-part-name		"&Part;")
(define ru-preface-name	"&Preface;")
(define ru-procedure-name	"&Procedure;")
(define ru-pubdate-name	"&Published;")
(define ru-reference-name	"&Reference;")
(define ru-refname-name	"&Name;")
(define ru-revhistory-name	"&RevisionHistory;")
(define ru-revision-name	"&Revision;")
(define ru-sect1-name		"&Section;")
(define ru-sect2-name		"&Section;")
(define ru-sect3-name		"&Section;")
(define ru-sect4-name		"&Section;")
(define ru-sect5-name		"&Section;")
(define ru-simplesect-name	"&Section;")
(define ru-seeie-name		"&See;")
(define ru-seealsoie-name	"&Seealso;")
(define ru-set-name		"&Set;")
(define ru-sidebar-name	"&Sidebar;")
(define ru-step-name		"&step;")
(define ru-table-name		"&Table;")
(define ru-tip-name		"&Tip;")
(define ru-toc-name		"&TableofContents;")
(define ru-warning-name	"&Warning;")

(define (gentext-ru-element-name gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract"))	ru-abstract-name)
     ((equal? name (normalize "appendix"))	ru-appendix-name)
     ((equal? name (normalize "article"))	ru-article-name)
     ((equal? name (normalize "bibliography"))	ru-bibliography-name)
     ((equal? name (normalize "book"))		ru-book-name)
     ((equal? name (normalize "calloutlist"))	ru-calloutlist-name)
     ((equal? name (normalize "caution"))	ru-caution-name)
     ((equal? name (normalize "chapter"))	ru-chapter-name)
     ((equal? name (normalize "copyright"))	ru-copyright-name)
     ((equal? name (normalize "dedication"))	ru-dedication-name)
     ((equal? name (normalize "edition"))	ru-edition-name)
     ((equal? name (normalize "equation"))	ru-equation-name)
     ((equal? name (normalize "example"))	ru-example-name)
     ((equal? name (normalize "figure"))	ru-figure-name)
     ((equal? name (normalize "glossary"))	ru-glossary-name)
     ((equal? name (normalize "glosssee"))	ru-glosssee-name)
     ((equal? name (normalize "glossseealso"))	ru-glossseealso-name)
     ((equal? name (normalize "important"))	ru-important-name)
     ((equal? name (normalize "index"))		ru-index-name)
     ((equal? name (normalize "setindex"))	ru-setindex-name)
     ((equal? name (normalize "isbn"))		ru-isbn-name)
     ((equal? name (normalize "legalnotice"))	ru-legalnotice-name)
     ((equal? name (normalize "msgaud"))	ru-msgaud-name)
     ((equal? name (normalize "msglevel"))	ru-msglevel-name)
     ((equal? name (normalize "msgorig"))	ru-msgorig-name)
     ((equal? name (normalize "note"))		ru-note-name)
     ((equal? name (normalize "part"))		ru-part-name)
     ((equal? name (normalize "preface"))	ru-preface-name)
     ((equal? name (normalize "procedure"))	ru-procedure-name)
     ((equal? name (normalize "pubdate"))	ru-pubdate-name)
     ((equal? name (normalize "reference"))	ru-reference-name)
     ((equal? name (normalize "refname"))	ru-refname-name)
     ((equal? name (normalize "revhistory"))	ru-revhistory-name)
     ((equal? name (normalize "revision"))	ru-revision-name)
     ((equal? name (normalize "sect1"))		ru-sect1-name)
     ((equal? name (normalize "sect2"))		ru-sect2-name)
     ((equal? name (normalize "sect3"))		ru-sect3-name)
     ((equal? name (normalize "sect4"))		ru-sect4-name)
     ((equal? name (normalize "sect5"))		ru-sect5-name)
     ((equal? name (normalize "simplesect"))    ru-simplesect-name)
     ((equal? name (normalize "seeie"))		ru-seeie-name)
     ((equal? name (normalize "seealsoie"))	ru-seealsoie-name)
     ((equal? name (normalize "set"))		ru-set-name)
     ((equal? name (normalize "sidebar"))	ru-sidebar-name)
     ((equal? name (normalize "step"))		ru-step-name)
     ((equal? name (normalize "table"))		ru-table-name)
     ((equal? name (normalize "tip"))		ru-tip-name)
     ((equal? name (normalize "toc"))		ru-toc-name)
     ((equal? name (normalize "warning"))	ru-warning-name)
     (else (let* ((msg (string-append "&unexpectedelementname;: " name))
		  (err (node-list-error msg (current-node))))
	     msg)))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-ru-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;

(define ru-equation-intra-label-sep "-")
(define ru-example-intra-label-sep "-")
(define ru-figure-intra-label-sep "-")
(define ru-procedure-intra-label-sep ".")
(define ru-refentry-intra-label-sep ".")
(define ru-reference-intra-label-sep ".")
(define ru-refname-intra-label-sep ", ")
(define ru-refsect1-intra-label-sep ".")
(define ru-refsect2-intra-label-sep ".")
(define ru-refsect3-intra-label-sep ".")
(define ru-sect1-intra-label-sep ".")
(define ru-sect2-intra-label-sep ".")
(define ru-sect3-intra-label-sep ".")
(define ru-sect4-intra-label-sep ".")
(define ru-sect5-intra-label-sep ".")
(define ru-step-intra-label-sep ".")
(define ru-table-intra-label-sep "-")
(define ru-_pagenumber-intra-label-sep "-")
(define ru-default-intra-label-sep "")

(define (gentext-ru-intra-label-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "equation"))	ru-equation-intra-label-sep)
     ((equal? name (normalize "example"))	ru-example-intra-label-sep)
     ((equal? name (normalize "figure"))	ru-figure-intra-label-sep)
     ((equal? name (normalize "procedure"))	ru-procedure-intra-label-sep)
     ((equal? name (normalize "refentry"))	ru-refentry-intra-label-sep)
     ((equal? name (normalize "reference"))	ru-reference-intra-label-sep)
     ((equal? name (normalize "refname"))	ru-refname-intra-label-sep)
     ((equal? name (normalize "refsect1"))	ru-refsect1-intra-label-sep)
     ((equal? name (normalize "refsect2"))	ru-refsect2-intra-label-sep)
     ((equal? name (normalize "refsect3"))	ru-refsect3-intra-label-sep)
     ((equal? name (normalize "sect1"))		ru-sect1-intra-label-sep)
     ((equal? name (normalize "sect2"))		ru-sect2-intra-label-sep)
     ((equal? name (normalize "sect3"))		ru-sect3-intra-label-sep)
     ((equal? name (normalize "sect4"))		ru-sect4-intra-label-sep)
     ((equal? name (normalize "sect5"))		ru-sect5-intra-label-sep)
     ((equal? name (normalize "step"))		ru-step-intra-label-sep)
     ((equal? name (normalize "table"))		ru-table-intra-label-sep)
     ((equal? name (normalize "_pagenumber"))	ru-_pagenumber-intra-label-sep)
     (else ru-default-intra-label-sep))))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;

(define ru-abstract-label-title-sep ": ")
(define ru-appendix-label-title-sep ". ")
(define ru-caution-label-title-sep "")
(define ru-chapter-label-title-sep ". ")
(define ru-equation-label-title-sep ". ")
(define ru-example-label-title-sep ". ")
(define ru-figure-label-title-sep ". ")
(define ru-footnote-label-title-sep ". ")
(define ru-glosssee-label-title-sep ": ")
(define ru-glossseealso-label-title-sep ": ")
(define ru-important-label-title-sep ": ")
(define ru-note-label-title-sep ": ")
(define ru-orderedlist-label-title-sep ". ")
(define ru-part-label-title-sep ". ")
(define ru-procedure-label-title-sep ". ")
(define ru-prefix-label-title-sep ". ")
(define ru-refentry-label-title-sep "")
(define ru-reference-label-title-sep ". ")
(define ru-refsect1-label-title-sep ". ")
(define ru-refsect2-label-title-sep ". ")
(define ru-refsect3-label-title-sep ". ")
(define ru-sect1-label-title-sep ". ")
(define ru-sect2-label-title-sep ". ")
(define ru-sect3-label-title-sep ". ")
(define ru-sect4-label-title-sep ". ")
(define ru-sect5-label-title-sep ". ")
(define ru-seeie-label-title-sep " ")
(define ru-seealsoie-label-title-sep " ")
(define ru-step-label-title-sep ". ")
(define ru-table-label-title-sep ". ")
(define ru-tip-label-title-sep ": ")
(define ru-warning-label-title-sep "")
(define ru-default-label-title-sep "")

(define (gentext-ru-label-title-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract")) ru-abstract-label-title-sep)
     ((equal? name (normalize "appendix")) ru-appendix-label-title-sep)
     ((equal? name (normalize "caution")) ru-caution-label-title-sep)
     ((equal? name (normalize "chapter")) ru-chapter-label-title-sep)
     ((equal? name (normalize "equation")) ru-equation-label-title-sep)
     ((equal? name (normalize "example")) ru-example-label-title-sep)
     ((equal? name (normalize "figure")) ru-figure-label-title-sep)
     ((equal? name (normalize "footnote")) ru-footnote-label-title-sep)
     ((equal? name (normalize "glosssee")) ru-glosssee-label-title-sep)
     ((equal? name (normalize "glossseealso")) ru-glossseealso-label-title-sep)
     ((equal? name (normalize "important")) ru-important-label-title-sep)
     ((equal? name (normalize "note")) ru-note-label-title-sep)
     ((equal? name (normalize "orderedlist")) ru-orderedlist-label-title-sep)
     ((equal? name (normalize "part")) ru-part-label-title-sep)
     ((equal? name (normalize "procedure")) ru-procedure-label-title-sep)
     ((equal? name (normalize "prefix")) ru-prefix-label-title-sep)
     ((equal? name (normalize "refentry")) ru-refentry-label-title-sep)
     ((equal? name (normalize "reference")) ru-reference-label-title-sep)
     ((equal? name (normalize "refsect1")) ru-refsect1-label-title-sep)
     ((equal? name (normalize "refsect2")) ru-refsect2-label-title-sep)
     ((equal? name (normalize "refsect3")) ru-refsect3-label-title-sep)
     ((equal? name (normalize "sect1")) ru-sect1-label-title-sep)
     ((equal? name (normalize "sect2")) ru-sect2-label-title-sep)
     ((equal? name (normalize "sect3")) ru-sect3-label-title-sep)
     ((equal? name (normalize "sect4")) ru-sect4-label-title-sep)
     ((equal? name (normalize "sect5")) ru-sect5-label-title-sep)
     ((equal? name (normalize "seeie")) ru-seeie-label-title-sep)
     ((equal? name (normalize "seealsoie")) ru-seealsoie-label-title-sep)
     ((equal? name (normalize "step")) ru-step-label-title-sep)
     ((equal? name (normalize "table")) ru-table-label-title-sep)
     ((equal? name (normalize "tip")) ru-tip-label-title-sep)
     ((equal? name (normalize "warning")) ru-warning-label-title-sep)
     (else ru-default-label-title-sep))))

(define (ru-set-label-number-format gind) "1")
(define (ru-book-label-number-format gind) "1")
(define (ru-prefix-label-number-format gind) "1")
(define (ru-part-label-number-format gind) "I")
(define (ru-chapter-label-number-format gind) "1")
(define (ru-appendix-label-number-format gind) "A")
(define (ru-reference-label-number-format gind) "I")
(define (ru-example-label-number-format gind) "1")
(define (ru-figure-label-number-format gind) "1")
(define (ru-table-label-number-format gind) "1")
(define (ru-procedure-label-number-format gind) "1")
(define (ru-step-label-number-format gind) "1")
(define (ru-refsect1-label-number-format gind) "1")
(define (ru-refsect2-label-number-format gind) "1")
(define (ru-refsect3-label-number-format gind) "1")
(define (ru-sect1-label-number-format gind) "1")
(define (ru-sect2-label-number-format gind) "1")
(define (ru-sect3-label-number-format gind) "1")
(define (ru-sect4-label-number-format gind) "1")
(define (ru-sect5-label-number-format gind) "1")
(define (ru-default-label-number-format gind) "1")

(define (ru-label-number-format gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "set")) (ru-set-label-number-format gind))
     ((equal? name (normalize "book")) (ru-book-label-number-format gind))
     ((equal? name (normalize "prefix")) (ru-prefix-label-number-format gind))
     ((equal? name (normalize "part")) (ru-part-label-number-format gind))
     ((equal? name (normalize "chapter")) (ru-chapter-label-number-format gind))
     ((equal? name (normalize "appendix")) (ru-appendix-label-number-format gind))
     ((equal? name (normalize "reference")) (ru-reference-label-number-format gind))
     ((equal? name (normalize "example")) (ru-example-label-number-format gind))
     ((equal? name (normalize "figure")) (ru-figure-label-number-format gind))
     ((equal? name (normalize "table")) (ru-table-label-number-format gind))
     ((equal? name (normalize "procedure")) (ru-procedure-label-number-format gind))
     ((equal? name (normalize "step")) (ru-step-label-number-format gind))
     ((equal? name (normalize "refsect1")) (ru-refsect1-label-number-format gind))
     ((equal? name (normalize "refsect2")) (ru-refsect2-label-number-format gind))
     ((equal? name (normalize "refsect3")) (ru-refsect3-label-number-format gind))
     ((equal? name (normalize "sect1")) (ru-sect1-label-number-format gind))
     ((equal? name (normalize "sect2")) (ru-sect2-label-number-format gind))
     ((equal? name (normalize "sect3")) (ru-sect3-label-number-format gind))
     ((equal? name (normalize "sect4")) (ru-sect4-label-number-format gind))
     ((equal? name (normalize "sect5")) (ru-sect5-label-number-format gind))
     (else (ru-default-label-number-format gind)))))

(define ($lot-title-ru$ gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond ((equal? name (normalize "table"))    "&ListofTables;")
	  ((equal? name (normalize "example"))  "&ListofExamples;")
	  ((equal? name (normalize "figure"))   "&ListofFigures;")
	  ((equal? name (normalize "equation")) "&ListofEquations;")
	  (else (let* ((msg (string-append "&ListofUnknown;: " name))
		       (err (node-list-error msg (current-node))))
		  msg)))))

(define %gentext-ru-start-quote% (dingbat "ldquo"))

(define %gentext-ru-end-quote% (dingbat "rdquo"))

(define %gentext-ru-by% "&by;") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-ru-edited-by% "&Editedby;")
                           ;; "Edited by" Jane Doe

(define %gentext-ru-page% "")

(define %gentext-ru-and% "&and;")

(define %gentext-ru-bibl-pages% "&Pgs;")

(define %gentext-ru-endnotes% "&Notes;")

(define %gentext-ru-table-endnotes% "&TableNotes;:")

(define %gentext-ru-index-see% "&See;")

(define %gentext-ru-index-seealso% "&SeeAlso;")

