;; $Id: dbl10n.dsl,v 1.1 1999/08/20 09:13:54 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; ----------------------------- Localization -----------------------------

;; If you create a new version of this file, please send it to
;; Norman Walsh, ndw@nwalsh.com

;; The generated text for cross references to elements.  See dblink.dsl
;; for a discussion of how substitution is performed on the %x and #x
;; keywords.
;;

(define %default-language% "usen")
(define %gentext-language% #f)
(define %gentext-use-xref-lang% #f)

(define ($lang$ #!optional (target (current-node)) (xref-context #f))
  (if %gentext-language%
      (case-fold-down %gentext-language%)
      (if (or xref-context %gentext-use-xref-lang%)
	  (let loop ((here target))
	    (if (node-list-empty? here)
		(case-fold-down %default-language%)
		(if (attribute-string "LANG" here)
		    (case-fold-down (attribute-string "LANG" here))
		    (loop (parent here)))))
	  (if (inherited-attribute-string "LANG")
	      (case-fold-down (inherited-attribute-string "LANG"))
	      (case-fold-down %default-language%)))))

(define (gentext-xref-strings target)
  (let ((giname (if (string? target) (normalize target) (gi target)))
	(lang   (if (string? target) ($lang$) ($lang$ target))))
    (case lang
      <![%l10n-bmno[ (("bmno") (gentext-bmno-xref-strings giname)) ]]>
      <![%l10n-dege[ (("dege") (gentext-dege-xref-strings giname)) ]]>
      <![%l10n-dk[   (("dk")   (gentext-dk-xref-strings giname)) ]]>
      <![%l10n-es[   (("es")   (gentext-es-xref-strings giname)) ]]>
      <![%l10n-fr[   (("fr")   (gentext-fr-xref-strings giname)) ]]>
      <![%l10n-it[   (("it")   (gentext-it-xref-strings giname)) ]]>
      <![%l10n-nl[   (("nl")   (gentext-nl-xref-strings giname)) ]]>
      <![%l10n-pl[   (("pl")   (gentext-pl-xref-strings giname)) ]]>
      <![%l10n-pt[   (("pt")   (gentext-pt-xref-strings giname)) ]]>
      <![%l10n-ru[   (("ru")   (gentext-ru-xref-strings giname)) ]]>
      <![%l10n-svse[ (("svse") (gentext-svse-xref-strings giname)) ]]>
      <![%l10n-usen[ (("usen") (gentext-usen-xref-strings giname)) ]]>
      (else (error "L10N ERROR: gentext-xref-strings")))))

(define (auto-xref-indirect-connector before) 
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") (bmno-auto-xref-indirect-connector before)) ]]>
    <![%l10n-dege[ (("dege") (dege-auto-xref-indirect-connector before)) ]]>
    <![%l10n-dk[   (("dk")   (dk-auto-xref-indirect-connector before)) ]]>
    <![%l10n-es[   (("es")   (es-auto-xref-indirect-connector before)) ]]>
    <![%l10n-fr[   (("fr")   (fr-auto-xref-indirect-connector before)) ]]>
    <![%l10n-it[   (("it")   (it-auto-xref-indirect-connector before)) ]]>
    <![%l10n-nl[   (("nl")   (nl-auto-xref-indirect-connector before)) ]]>
    <![%l10n-pl[   (("pl")   (pl-auto-xref-indirect-connector before)) ]]>
    <![%l10n-pt[   (("pt")   (pt-auto-xref-indirect-connector before)) ]]>
    <![%l10n-ru[   (("ru")   (ru-auto-xref-indirect-connector before)) ]]>
    <![%l10n-svse[ (("svse") (svse-auto-xref-indirect-connector before)) ]]>
    <![%l10n-usen[ (("usen") (usen-auto-xref-indirect-connector before)) ]]>
    (else (error "L10N ERROR: auto-xref-indirect-connector"))))

(define (generate-toc-in-front)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %generate-bmno-toc-in-front%) ]]>
    <![%l10n-dege[ (("dege") %generate-dege-toc-in-front%) ]]>
    <![%l10n-dk[  (("dk") %generate-dk-toc-in-front%) ]]>
    <![%l10n-es[   (("es")   %generate-es-toc-in-front%) ]]>
    <![%l10n-fr[   (("fr")   %generate-fr-toc-in-front%) ]]>
    <![%l10n-it[   (("it")   %generate-it-toc-in-front%) ]]>
    <![%l10n-nl[   (("nl")   %generate-nl-toc-in-front%) ]]>
    <![%l10n-pl[   (("pl")   %generate-pl-toc-in-front%) ]]>
    <![%l10n-pt[   (("pt")   %generate-pt-toc-in-front%) ]]>
    <![%l10n-ru[   (("ru")   %generate-ru-toc-in-front%) ]]>
    <![%l10n-svse[ (("svse") %generate-svse-toc-in-front%) ]]>
    <![%l10n-usen[ (("usen") %generate-usen-toc-in-front%) ]]>
    (else (error "L10N ERROR: generate-toc-in-front"))))

(define (gentext-element-name target)
  (let ((giname (if (string? target) (normalize target) (gi target)))
	(lang   (if (string? target) ($lang$) ($lang$ target #t))))
    (case lang
      <![%l10n-bmno[ (("bmno") (gentext-bmno-element-name giname)) ]]>
      <![%l10n-dege[ (("dege") (gentext-dege-element-name giname)) ]]>
      <![%l10n-dk[   (("dk")   (gentext-dk-element-name giname)) ]]>
      <![%l10n-it[   (("it")   (gentext-it-element-name giname)) ]]>
      <![%l10n-es[   (("es")   (gentext-es-element-name giname)) ]]>
      <![%l10n-fr[   (("fr")   (gentext-fr-element-name giname)) ]]>
      <![%l10n-nl[   (("nl")   (gentext-nl-element-name giname)) ]]>
      <![%l10n-pl[   (("pl")   (gentext-pl-element-name giname)) ]]>
      <![%l10n-pt[   (("pt")   (gentext-pt-element-name giname)) ]]>
      <![%l10n-ru[   (("ru")   (gentext-ru-element-name giname)) ]]>
      <![%l10n-svse[ (("svse") (gentext-svse-element-name giname)) ]]>
      <![%l10n-usen[ (("usen") (gentext-usen-element-name giname)) ]]>
      (else (error "L10N ERROR: gentext-element-name")))))

(define (gentext-element-name-space target)
  (let ((giname (if (string? target) (normalize target) (gi target)))
	(lang   (if (string? target) ($lang$) ($lang$ target))))
    (case lang
      <![%l10n-bmno[ (("bmno") (gentext-bmno-element-name-space giname)) ]]>
      <![%l10n-dege[ (("dege") (gentext-dege-element-name-space giname)) ]]>
      <![%l10n-dk[   (("dk")   (gentext-dk-element-name-space giname)) ]]>
      <![%l10n-es[   (("es")   (gentext-es-element-name-space giname)) ]]>
      <![%l10n-fr[   (("fr")   (gentext-fr-element-name-space giname)) ]]>
      <![%l10n-it[   (("it")   (gentext-it-element-name-space giname)) ]]>
      <![%l10n-nl[   (("nl")   (gentext-nl-element-name-space giname)) ]]>
      <![%l10n-pl[   (("pl")   (gentext-pl-element-name-space giname)) ]]>
      <![%l10n-pt[   (("pt")   (gentext-pt-element-name-space giname)) ]]>
      <![%l10n-ru[   (("ru")   (gentext-ru-element-name-space giname)) ]]>
      <![%l10n-svse[ (("svse") (gentext-svse-element-name-space giname)) ]]>
      <![%l10n-usen[ (("usen") (gentext-usen-element-name-space giname)) ]]>
      (else (error "L10N ERROR: gentext-element-name-space")))))

(define (gentext-intra-label-sep target)
  (let ((giname (if (string? target) (normalize target) (gi target)))
	(lang   (if (string? target) ($lang$) ($lang$ target))))
    (case lang
      <![%l10n-bmno[ (("bmno") (gentext-bmno-intra-label-sep giname)) ]]>
      <![%l10n-dege[ (("dege") (gentext-dege-intra-label-sep giname)) ]]>
      <![%l10n-dk[   (("dk")   (gentext-dk-intra-label-sep giname)) ]]>
      <![%l10n-es[   (("es")   (gentext-es-intra-label-sep giname)) ]]>
      <![%l10n-fr[   (("fr")   (gentext-fr-intra-label-sep giname)) ]]>
      <![%l10n-it[   (("it")   (gentext-it-intra-label-sep giname)) ]]>
      <![%l10n-nl[   (("nl")   (gentext-nl-intra-label-sep giname)) ]]>
      <![%l10n-pl[   (("pl")   (gentext-pl-intra-label-sep giname)) ]]>
      <![%l10n-pt[   (("pt")   (gentext-pt-intra-label-sep giname)) ]]>
      <![%l10n-ru[   (("ru")   (gentext-ru-intra-label-sep giname)) ]]>
      <![%l10n-svse[ (("svse") (gentext-svse-intra-label-sep giname)) ]]>
      <![%l10n-usen[ (("usen") (gentext-usen-intra-label-sep giname)) ]]>
      (else (error "L10N ERROR: gentext-intra-label-sep")))))

(define (gentext-label-title-sep target)
  (let ((giname (if (string? target) (normalize target) (gi target)))
	(lang   (if (string? target) ($lang$) ($lang$ target))))
    (case lang
      <![%l10n-bmno[ (("bmno") (gentext-bmno-label-title-sep giname)) ]]>
      <![%l10n-dege[ (("dege") (gentext-dege-label-title-sep giname)) ]]>
      <![%l10n-dk[   (("dk")   (gentext-dk-label-title-sep giname)) ]]>
      <![%l10n-es[   (("es")   (gentext-es-label-title-sep giname)) ]]>
      <![%l10n-fr[   (("fr")   (gentext-fr-label-title-sep giname)) ]]>
      <![%l10n-it[   (("it")   (gentext-it-label-title-sep giname)) ]]>
      <![%l10n-nl[   (("nl")   (gentext-nl-label-title-sep giname)) ]]>
      <![%l10n-pl[   (("pl")   (gentext-pl-label-title-sep giname)) ]]>
      <![%l10n-pt[   (("pt")   (gentext-pt-label-title-sep giname)) ]]>
      <![%l10n-ru[   (("ru")   (gentext-ru-label-title-sep giname)) ]]>
      <![%l10n-svse[ (("svse") (gentext-svse-label-title-sep giname)) ]]>
      <![%l10n-usen[ (("usen") (gentext-usen-label-title-sep giname)) ]]>
      (else (error "L10N ERROR: gentext-label-title-sep")))))

(define (label-number-format target)
  (let ((giname (if (string? target) (normalize target) (gi target)))
	(lang   (if (string? target) ($lang$) ($lang$ target))))
    (case lang
      <![%l10n-bmno[ (("bmno") (bmno-label-number-format target)) ]]>
      <![%l10n-dege[ (("dege") (dege-label-number-format target)) ]]>
      <![%l10n-dk[   (("dk")   (dk-label-number-format target)) ]]>
      <![%l10n-es[   (("es")   (es-label-number-format target)) ]]>
      <![%l10n-fr[   (("fr")   (fr-label-number-format target)) ]]>
      <![%l10n-it[   (("it")   (it-label-number-format target)) ]]>
      <![%l10n-nl[   (("nl")   (nl-label-number-format target)) ]]>
      <![%l10n-pl[   (("pl")   (pl-label-number-format target)) ]]>
      <![%l10n-pt[   (("pt")   (pt-label-number-format target)) ]]>
      <![%l10n-ru[   (("ru")   (ru-label-number-format target)) ]]>
      <![%l10n-svse[ (("svse") (svse-label-number-format target)) ]]>
      <![%l10n-usen[ (("usen") (usen-label-number-format target)) ]]>
      (else (error "L10N ERROR: gentext-label-title-sep")))))

(define ($lot-title$ lotgi)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") ($lot-title-bmno$ lotgi)) ]]>
    <![%l10n-dege[ (("dege") ($lot-title-dege$ lotgi)) ]]>
    <![%l10n-dk[   (("dk")   ($lot-title-dk$ lotgi)) ]]>
    <![%l10n-es[   (("es")   ($lot-title-es$ lotgi)) ]]>
    <![%l10n-fr[   (("fr")   ($lot-title-fr$ lotgi)) ]]>
    <![%l10n-it[   (("it")   ($lot-title-it$ lotgi)) ]]>
    <![%l10n-nl[   (("nl")   ($lot-title-nl$ lotgi)) ]]>
    <![%l10n-pl[   (("pl")   ($lot-title-pl$ lotgi)) ]]>
    <![%l10n-pt[   (("pt")   ($lot-title-pt$ lotgi)) ]]>
    <![%l10n-ru[   (("ru")   ($lot-title-ru$ lotgi)) ]]>
    <![%l10n-svse[ (("svse") ($lot-title-svse$ lotgi)) ]]>
    <![%l10n-usen[ (("usen") ($lot-title-usen$ lotgi)) ]]>
    (else (error "L10N ERROR: $lot-title$"))))

(define (gentext-start-quote)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-start-quote%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-start-quote%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-start-quote%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-start-quote%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-start-quote%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-start-quote%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-start-quote%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-start-quote%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-start-quote%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-start-quote%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-start-quote%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-start-quote%) ]]>
    (else (error "L10N ERROR: gentext-start-quote"))))

(define (gentext-end-quote)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-end-quote%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-end-quote%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-end-quote%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-end-quote%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-end-quote%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-end-quote%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-end-quote%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-end-quote%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-end-quote%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-end-quote%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-end-quote%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-end-quote%) ]]>
    (else (error "L10N ERROR: gentext-end-quote"))))

(define (gentext-by)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-by%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-by%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-by%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-by%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-by%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-by%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-by%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-by%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-by%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-by%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-by%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-by%) ]]>
    (else (error "L10N ERROR: gentext-by"))))

(define (gentext-edited-by)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-edited-by%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-edited-by%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-edited-by%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-edited-by%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-edited-by%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-edited-by%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-edited-by%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-edited-by%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-edited-by%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-edited-by%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-edited-by%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-edited-by%) ]]>
    (else (error "L10N ERROR: gentext-edited-by"))))

(define (gentext-page)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-page%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-page%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-page%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-page%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-page%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-page%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-page%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-page%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-page%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-page%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-page%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-page%) ]]>
    (else (error "L10N ERROR: gentext-page"))))

(define (gentext-and)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-and%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-and%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-and%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-and%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-and%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-and%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-and%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-and%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-and%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-and%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-and%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-and%) ]]>
    (else (error "L10N ERROR: gentext-and"))))

(define (gentext-bibl-pages)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-bibl-pages%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-bibl-pages%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-bibl-pages%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-bibl-pages%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-bibl-pages%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-bibl-pages%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-bibl-pages%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-bibl-pages%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-bibl-pages%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-bibl-pages%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-bibl-pages%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-bibl-pages%) ]]>
    (else (error "L10N ERROR: gentext-bibl-pages"))))

(define (gentext-endnotes)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-endnotes%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-endnotes%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-endnotes%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-endnotes%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-endnotes%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-endnotes%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-endnotes%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-endnotes%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-endnotes%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-endnotes%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-endnotes%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-endnotes%) ]]>
    (else (error "L10N ERROR: gentext-endnotes"))))

(define (gentext-table-endnotes)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-table-endnotes%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-table-endnotes%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-table-endnotes%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-table-endnotes%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-table-endnotes%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-table-endnotes%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-table-endnotes%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-table-endnotes%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-table-endnotes%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-table-endnotes%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-table-endnotes%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-table-endnotes%) ]]>
    (else (error "L10N ERROR: gentext-table-endnotes"))))

(define (gentext-index-see)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-index-see%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-index-see%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-index-see%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-index-see%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-index-see%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-index-see%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-index-see%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-index-see%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-index-see%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-index-see%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-index-see%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-index-see%) ]]>
    (else (error "L10N ERROR: gentext-index-see"))))

(define (gentext-index-seealso)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") %gentext-bmno-index-seealso%) ]]>
    <![%l10n-dege[ (("dege") %gentext-dege-index-seealso%) ]]>
    <![%l10n-dk[  (("dk") %gentext-dk-index-seealso%) ]]>
    <![%l10n-es[   (("es")   %gentext-es-index-seealso%) ]]>
    <![%l10n-fr[   (("fr")   %gentext-fr-index-seealso%) ]]>
    <![%l10n-it[   (("it")   %gentext-it-index-seealso%) ]]>
    <![%l10n-nl[   (("nl")   %gentext-nl-index-seealso%) ]]>
    <![%l10n-pl[   (("pl")   %gentext-pl-index-seealso%) ]]>
    <![%l10n-pt[   (("pt")   %gentext-pt-index-seealso%) ]]>
    <![%l10n-ru[   (("ru")   %gentext-ru-index-seealso%) ]]>
    <![%l10n-svse[ (("svse") %gentext-svse-index-seealso%) ]]>
    <![%l10n-usen[ (("usen") %gentext-usen-index-seealso%) ]]>
    (else (error "L10N ERROR: gentext-index-seealso"))))

