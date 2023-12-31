<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % lat1 PUBLIC "ISO 8879:1986//ENTITIES Added Latin 1//EN">
%lat1;
<!ENTITY % bmno.words SYSTEM "../common/dbl1bmno.ent">
%bmno.words;
<!ENTITY cmn.bmno SYSTEM "../common/dbl1bmno.dsl">
]>

<style-sheet>
<style-specification id="docbook-l10n-bmno">
<style-specification-body>

;; $Id: dbl1bmno.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

&cmn.bmno;

(define (gentext-bmno-nav-prev prev) 
  (make sequence (literal "Forrige")))

(define (gentext-bmno-nav-prevsib prevsib) 
  (make sequence (literal "Raskt Bakover")))

(define (gentext-bmno-nav-nextsib nextsib)
  (make sequence (literal "Raskt Fremover")))

(define (gentext-bmno-nav-next next)
  (make sequence (literal "Neste")))

(define (gentext-bmno-nav-up up)
  (make sequence (literal "Opp")))

(define (gentext-bmno-nav-home home)
  (make sequence (literal "Hjem")))

</style-specification-body>
</style-specification>
</style-sheet>
