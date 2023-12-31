<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
]>

<style-sheet>
<style-specification id="docbook-plain" use="docbook">
<style-specification-body>

;; $Id: plain.dsl,v 1.1 1999/08/20 09:13:58 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;
;; Example of a customization layer on top of the modular docbook style
;; sheet.  Definitions inserted in this file take precedence over 
;; definitions in the 'use'd stylesheet(s).

(define %generate-set-titlepage% #f)
(define %generate-book-titlepage% #f)
(define %generate-part-titlepage% #f)
(define %generate-reference-titlepage% #f)
(define %generate-article-titlepage% #f)

(define %generate-set-toc% #f)
(define %generate-book-toc% #f)
(define %generate-part-toc% #f)
(define %generate-reference-toc% #f)
(define %generate-article-toc% #f)

(define %generate-book-lot-list% '())

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook.dsl">

</style-sheet>
