;; $Id: dbl10n.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/docbook/dsssl/
;;

(define (gentext-nav-prev prev) 
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") (gentext-bmno-nav-prev prev)) ]]>
    <![%l10n-dege[ (("dege") (gentext-dege-nav-prev prev)) ]]>
    <![%l10n-es[   (("es")   (gentext-es-nav-prev prev)) ]]>
    <![%l10n-fr[   (("fr")   (gentext-fr-nav-prev prev)) ]]>
    <![%l10n-it[   (("it")   (gentext-it-nav-prev prev)) ]]>
    <![%l10n-nl[   (("nl")   (gentext-nl-nav-prev prev)) ]]>
    <![%l10n-pl[   (("pl")   (gentext-pl-nav-prev prev)) ]]>
    <![%l10n-pt[   (("pt")   (gentext-pt-nav-prev prev)) ]]>
    <![%l10n-ru[   (("ru")   (gentext-ru-nav-prev prev)) ]]>
    <![%l10n-usen[ (("usen") (gentext-usen-nav-prev prev)) ]]>
    (else (error "L10N ERROR: gentext-nav-prev"))))

(define (gentext-nav-prevsib prevsib) 
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") (gentext-bmno-nav-prevsib prevsib)) ]]>
    <![%l10n-dege[ (("dege") (gentext-dege-nav-prevsib prevsib)) ]]>
    <![%l10n-es[   (("es")   (gentext-es-nav-prevsib prevsib)) ]]>
    <![%l10n-fr[   (("fr")   (gentext-fr-nav-prevsib prevsib)) ]]>
    <![%l10n-it[   (("it")   (gentext-it-nav-prevsib prevsib)) ]]>
    <![%l10n-nl[   (("nl")   (gentext-nl-nav-prevsib prevsib)) ]]>
    <![%l10n-pl[   (("pl")   (gentext-pl-nav-prevsib prevsib)) ]]>
    <![%l10n-pt[   (("pt")   (gentext-pt-nav-prevsib prevsib)) ]]>
    <![%l10n-ru[   (("ru")   (gentext-ru-nav-prevsib prevsib)) ]]>
    <![%l10n-usen[ (("usen") (gentext-usen-nav-prevsib prevsib)) ]]>
    (else (error "L10N ERROR: gentext-nav-prevsib "))))

(define (gentext-nav-nextsib nextsib)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") (gentext-bmno-nav-nextsib nextsib)) ]]>
    <![%l10n-dege[ (("dege") (gentext-dege-nav-nextsib nextsib)) ]]>
    <![%l10n-es[   (("es")   (gentext-es-nav-nextsib nextsib)) ]]>
    <![%l10n-fr[   (("fr")   (gentext-fr-nav-nextsib nextsib)) ]]>
    <![%l10n-it[   (("it")   (gentext-it-nav-nextsib nextsib)) ]]>
    <![%l10n-nl[   (("nl")   (gentext-nl-nav-nextsib nextsib)) ]]>
    <![%l10n-pl[   (("pl")   (gentext-pl-nav-nextsib nextsib)) ]]>
    <![%l10n-pt[   (("pt")   (gentext-pt-nav-nextsib nextsib)) ]]>
    <![%l10n-ru[   (("ru")   (gentext-ru-nav-nextsib nextsib)) ]]>
    <![%l10n-usen[ (("usen") (gentext-usen-nav-nextsib nextsib)) ]]>
    (else (error "L10N ERROR: gentext-nav-nextsib"))))

(define (gentext-nav-next next)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") (gentext-bmno-nav-next next)) ]]>
    <![%l10n-dege[ (("dege") (gentext-dege-nav-next next)) ]]>
    <![%l10n-es[   (("es")   (gentext-es-nav-next next)) ]]>
    <![%l10n-fr[   (("fr")   (gentext-fr-nav-next next)) ]]>
    <![%l10n-it[   (("it")   (gentext-it-nav-next next)) ]]>
    <![%l10n-nl[   (("nl")   (gentext-nl-nav-next next)) ]]>
    <![%l10n-pl[   (("pl")   (gentext-pl-nav-next next)) ]]>
    <![%l10n-pt[   (("pt")   (gentext-pt-nav-next next)) ]]>
    <![%l10n-ru[   (("ru")   (gentext-ru-nav-next next)) ]]>
    <![%l10n-usen[ (("usen") (gentext-usen-nav-next next)) ]]>
    (else (error "L10N ERROR: gentext-nav-next"))))

(define (gentext-nav-up up)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") (gentext-bmno-nav-up up)) ]]>
    <![%l10n-dege[ (("dege") (gentext-dege-nav-up up)) ]]>
    <![%l10n-es[   (("es")   (gentext-es-nav-up up)) ]]>
    <![%l10n-fr[   (("fr")   (gentext-fr-nav-up up)) ]]>
    <![%l10n-it[   (("it")   (gentext-it-nav-up up)) ]]>
    <![%l10n-nl[   (("nl")   (gentext-nl-nav-up up)) ]]>
    <![%l10n-pl[   (("pl")   (gentext-pl-nav-up up)) ]]>
    <![%l10n-pt[   (("pt")   (gentext-pt-nav-up up)) ]]>
    <![%l10n-ru[   (("ru")   (gentext-ru-nav-up up)) ]]>
    <![%l10n-usen[ (("usen") (gentext-usen-nav-up up)) ]]>
    (else (error "L10N ERROR: gentext-nav-up"))))

(define (gentext-nav-home home)
  (case ($lang$)
    <![%l10n-bmno[ (("bmno") (gentext-bmno-nav-home home)) ]]>
    <![%l10n-dege[ (("dege") (gentext-dege-nav-home home)) ]]>
    <![%l10n-es[   (("es")   (gentext-es-nav-home home)) ]]>
    <![%l10n-fr[   (("fr")   (gentext-fr-nav-home home)) ]]>
    <![%l10n-it[   (("it")   (gentext-it-nav-home home)) ]]>
    <![%l10n-nl[   (("nl")   (gentext-nl-nav-home home)) ]]>
    <![%l10n-pl[   (("pl")   (gentext-pl-nav-home home)) ]]>
    <![%l10n-pt[   (("pt")   (gentext-pt-nav-home home)) ]]>
    <![%l10n-ru[   (("ru")   (gentext-ru-nav-home home)) ]]>
    <![%l10n-usen[ (("usen") (gentext-usen-nav-home home)) ]]>
    (else (error "L10N ERROR: gentext-nav-home"))))

