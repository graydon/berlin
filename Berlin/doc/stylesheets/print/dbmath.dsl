;; $Id: dbmath.dsl,v 1.1 1999/08/20 09:13:57 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(element equation
  (if (node-list-empty? (select-elements (children (current-node)) 
					 (normalize "title")))
      ($informal-object$)
      ($formal-object$)))

(element (equation title) (empty-sosofo))
(element (equation alt) (empty-sosofo))
(element (equation graphic) 
  (make paragraph
    space-before: 0pt
    space-after: 0pt
    ($img$ (current-node) #t)))

(element informalequation ($informal-object$))
(element (informalequation alt) (empty-sosofo))
(element (informalequation graphic) 
  (make paragraph
    space-before: 0pt
    space-after: 0pt
    ($img$ (current-node) #t)))

(element inlineequation ($inline-object$))
(element (inlineequation alt) (empty-sosofo))
(element (inlineequation graphic) 
  (make sequence
    ($img$ (current-node) #f)))

