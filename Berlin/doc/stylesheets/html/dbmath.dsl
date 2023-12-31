;; $Id: dbmath.dsl,v 1.1 1999/08/20 09:13:56 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(element equation ($semiformal-object$))

(element (equation title) (empty-sosofo))
(element (equation alt) (empty-sosofo))
(element (equation graphic)
  (let ((alttag (select-elements (children (parent)) (normalize "alt"))))
    (if alttag
	($img$ (current-node) (data alttag))
	($img$))))

(element informalequation ($informal-object$))
(element (informalequation alt) (empty-sosofo))
(element (informalequation graphic) 
  (let ((alttag (select-elements (children (parent)) (normalize "alt"))))
    (if alttag
	($img$ (current-node) (data alttag))
	($img$))))

(element inlineequation ($inline-object$))
(element (inlineequation alt) (empty-sosofo))
(element (inlineequation graphic) 
  (let ((alttag (select-elements (children (parent)) (normalize "alt"))))
    (if alttag
	($img$ (current-node) (data alttag))
	($img$))))
