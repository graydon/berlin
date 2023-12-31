<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % fr.words SYSTEM "../common/dbl1fr.ent">
%fr.words;
<!ENTITY cmn.fr SYSTEM "../common/dbl1fr.dsl">
]>

<style-sheet>
<style-specification id="docbook-l10n-fr">
<style-specification-body>

;; $Id: dbl1fr.dsl,v 1.1 1999/08/20 09:13:55 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

&cmn.fr;

(define (gentext-fr-nav-prev prev) 
  (make sequence (literal "Prev")))

(define (gentext-fr-nav-prevsib prevsib) 
  (make sequence (literal "Fast Backward")))

(define (gentext-fr-nav-nextsib nextsib)
  (make sequence (literal "Fast Forward")))

(define (gentext-fr-nav-next next)
  (make sequence (literal "Next")))

(define (gentext-fr-nav-up up)
  (make sequence (literal "Up")))

(define (gentext-fr-nav-home home)
  (make sequence (literal "Home")))

</style-specification-body>
</style-specification>
</style-sheet>
