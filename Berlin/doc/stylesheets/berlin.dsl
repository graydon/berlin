<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
]]>
]>


<!-- This is a driver file for the berlin project's documentation tree      -->
<!-- it is a customization layer for norm walsh's modular stylesheets       -->
<!-- including extensions to handle class and interface synopses            -->
<!-- if you find bugs, please email graydon@pobox.com                       -->


<style-sheet>
<style-specification id="print" use="docbook">
<style-specification-body> 

;; customizations for the print stylesheet

</style-specification-body>
</style-specification>
<style-specification id="html" use="docbook">
<style-specification-body> 

;; customizations for the HTML stylesheet
;; mostly dealing with rendering class definitions.
  
(element classsynopsis 
	 (make element gi: "table"
	       attributes: '(("border" "0")
			     ("bgcolor" "eeeeee")
			     ("cellpadding" "15"))
	       
	       (make element gi: "tr"
		     (make element gi: "td"
			   (process-children)
			   (literal "}")))))

(element (classsynopsis classname)
	 (make sequence
	       (literal " class ")
	       (make element gi: "b"
		     (process-children))
	       (literal " ")
	       (if (equal? (gi (ifollow (current-node))) (normalize "superclass"))
		   (literal ": ")
		   (literal " { "))))

(element (classsynopsis superclass)
	 (make sequence
	       (make element gi: "i"
		     (process-children))
	       (if (equal? (gi (ifollow (current-node))) (normalize "superclass"))
		   (literal ", ")
		   (literal " { "))))


(element modifier
	 (make sequence
	       (make element gi: "i"
		     (process-children))
	       (literal " ")))
	       
(element methodsynopsis ($informal-object$))

(element fieldsynopsis ($informal-object$))

(element (methodsynopsis void)
	 (literal " void "))

(element methodname
	 (make element gi: "b"
	       ($inline-object$)))

(element exception
	 (make sequence
	       (if (equal? (gi (ipreced (current-node))) (normalize "exception"))	     
		   (literal ", ")
		   (literal " throws "))
	       (process-children)))



(element initializer
	 (make sequence
	       (literal " = ")
	       (process-children)))

(element (fieldsynopsis varname)
	 ($inline-object$))

(element methodparam
	 (make sequence

	       (if (equal? (gi (ipreced (current-node))) (normalize "methodparam"))
		   (empty-sosofo)
		   (literal " ("))
	       
	       (process-node-list (select-elements
				   (children
				    (current-node))
				   (normalize
				    "type")))
	       
	       (literal " ")

	       (process-node-list (select-elements
				   (children
				    (current-node))
				   (normalize
				    "parameter")))
	       
	       (if (equal? (gi (ifollow (current-node))) (normalize "methodparam"))
		   (literal ", ")
		   (literal ") "))))


	 
  
</style-specification-body>
</style-specification>
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>
