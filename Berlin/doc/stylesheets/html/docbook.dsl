<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % l10n-bmno "INCLUDE">
<!ENTITY % l10n-dege "INCLUDE">
<!ENTITY % l10n-dk   "INCLUDE">
<!ENTITY % l10n-es   "INCLUDE">
<!ENTITY % l10n-fr   "INCLUDE">
<!ENTITY % l10n-it   "INCLUDE">
<!ENTITY % l10n-nl   "INCLUDE">
<!ENTITY % l10n-pl   "INCLUDE">
<!ENTITY % l10n-pt   "INCLUDE">
<!ENTITY % l10n-ru   "INCLUDE">
<!ENTITY % l10n-svse "INCLUDE">
<!ENTITY % l10n-usen "INCLUDE">

<![%l10n-bmno;[
<!ENTITY dbl1bmno SYSTEM "dbl1bmno.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1bmno SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-dege;[
<!ENTITY dbl1dege SYSTEM "dbl1dege.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1dege SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-dk;[
<!ENTITY dbl1dk   SYSTEM "dbl1dk.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1dk   SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-es;[
<!ENTITY dbl1es   SYSTEM "dbl1es.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1es   SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-fr;[
<!ENTITY dbl1fr   SYSTEM "dbl1fr.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1fr   SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-it;[
<!ENTITY dbl1it   SYSTEM "dbl1it.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1it   SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-nl;[
<!ENTITY dbl1nl   SYSTEM "dbl1nl.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1nl   SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-pl;[
<!ENTITY dbl1pl SYSTEM "dbl1pl.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1pl SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-pt;[
<!ENTITY dbl1pt SYSTEM "dbl1pt.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1pt SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-ru;[
<!ENTITY dbl1ru   SYSTEM "dbl1ru.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1ru   SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-svse;[
<!ENTITY dbl1svse SYSTEM "dbl1svse.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1svse SYSTEM "dbl1null.dsl" CDATA DSSSL>

<![%l10n-usen;[
<!ENTITY dbl1usen SYSTEM "dbl1usen.dsl" CDATA DSSSL>
]]>
<!ENTITY dbl1usen SYSTEM "dbl1null.dsl" CDATA DSSSL>

<!ENTITY cmndbl10n.dsl  SYSTEM "../common/dbl10n.dsl">
<!ENTITY dbl10n.dsl  SYSTEM "dbl10n.dsl">

<!ENTITY % dsssl-source "INCLUDE">
<!ENTITY % dsssl-documentation "IGNORE">

<!ENTITY dbcommon.dsl SYSTEM "../common/dbcommon.dsl">
<!ENTITY dbctable.dsl SYSTEM "../common/dbtable.dsl">
<!ENTITY dblib.dsl    PUBLIC "-//Norman Walsh//DOCUMENT DSSSL Library//EN">
<!ENTITY dbparam.dsl  SYSTEM "dbparam.dsl">
<!ENTITY dbadmon.dsl  SYSTEM "dbadmon.dsl">
<!ENTITY dbautoc.dsl  SYSTEM "dbautoc.dsl">
<!ENTITY dbbibl.dsl   SYSTEM "dbbibl.dsl">
<!ENTITY dbblock.dsl  SYSTEM "dbblock.dsl">
<!ENTITY dbcallou.dsl SYSTEM "dbcallou.dsl">
<!ENTITY dbcompon.dsl SYSTEM "dbcompon.dsl">
<!ENTITY dbdivis.dsl  SYSTEM "dbdivis.dsl">
<!ENTITY dbfootn.dsl  SYSTEM "dbfootn.dsl">
<!ENTITY dbgloss.dsl  SYSTEM "dbgloss.dsl">
<!ENTITY dbgraph.dsl  SYSTEM "dbgraph.dsl">
<!ENTITY dbhtml.dsl   SYSTEM "dbhtml.dsl">
<!ENTITY dbindex.dsl  SYSTEM "dbindex.dsl">
<!ENTITY dbinfo.dsl   SYSTEM "dbinfo.dsl">
<!ENTITY dbinline.dsl SYSTEM "dbinline.dsl">
<!ENTITY dblink.dsl   SYSTEM "dblink.dsl">
<!ENTITY dblists.dsl  SYSTEM "dblists.dsl">
<!ENTITY dblot.dsl    SYSTEM "dblot.dsl">
<!ENTITY dbmath.dsl   SYSTEM "dbmath.dsl">
<!ENTITY dbmsgset.dsl SYSTEM "dbmsgset.dsl">
<!ENTITY dbnavig.dsl  SYSTEM "dbnavig.dsl">
<!ENTITY dbprocdr.dsl SYSTEM "dbprocdr.dsl">
<!ENTITY dbrfntry.dsl SYSTEM "dbrfntry.dsl">
<!ENTITY dbsect.dsl   SYSTEM "dbsect.dsl">
<!ENTITY dbsynop.dsl  SYSTEM "dbsynop.dsl">
<!ENTITY dbtable.dsl  SYSTEM "dbtable.dsl">
<!ENTITY dbtitle.dsl  SYSTEM "dbtitle.dsl">
<!ENTITY dbttlpg.dsl  SYSTEM "dbttlpg.dsl">
<!ENTITY dbverb.dsl   SYSTEM "dbverb.dsl">
<!ENTITY version.dsl  SYSTEM "version.dsl">
]>

<style-sheet>
<style-specification id="docbook" use="usen pl pt dege dk bmno ru es fr it nl pl svse">
<style-specification-body>

;; $Id: docbook.dsl,v 1.1 1999/08/20 09:13:56 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(declare-flow-object-class element
  "UNREGISTERED::James Clark//Flow Object Class::element")

(declare-flow-object-class empty-element
  "UNREGISTERED::James Clark//Flow Object Class::empty-element")

(declare-flow-object-class document-type
  "UNREGISTERED::James Clark//Flow Object Class::document-type")

(declare-flow-object-class processing-instruction
  "UNREGISTERED::James Clark//Flow Object Class::processing-instruction")

(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")

(declare-flow-object-class entity-ref
  "UNREGISTERED::James Clark//Flow Object Class::entity-ref")

(declare-flow-object-class formatting-instruction
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")

(declare-characteristic preserve-sdata?
  "UNREGISTERED::James Clark//Characteristic::preserve-sdata?" #t)

(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))

(define read-entity
  (external-procedure "UNREGISTERED::James Clark//Procedure::read-entity"))

(define all-element-number
  (external-procedure "UNREGISTERED::James Clark//Procedure::all-element-number"))

(root
 (make sequence
;   (literal
;    (debug (node-property 'gi
;			  (node-property 'document-element (current-node)))))
;(define (docelem node)
;  (node-propety 'document-element 
;    (node-property 'grove-root node)))
   (process-children)
   (with-mode manifest
     (process-children))
   (if html-index
       (with-mode htmlindex
	 (process-children))
       (empty-sosofo))))

(mode manifest
  ;; this mode is really just a hack to get at the root element
  (root (process-children))

  (default 
    (if (node-list=? (current-node) (sgml-root-element))
	(if html-manifest
	    (make entity
	      system-id: (html-entity-file html-manifest-filename)
	      (let loop ((node (current-node)))
		(if (node-list-empty? node)
		    (empty-sosofo)
		    (make sequence
		      (make formatting-instruction data: (html-file node))
		      (make formatting-instruction data: "
")
		      (loop (next-chunk-element node))))))
	    (empty-sosofo))
	(empty-sosofo))))

;; Make text that comes from unimplemented tags easy to spot
(default
  (make element gi: "FONT"
	attributes: '(("COLOR" "RED"))
	(process-children)))

&dblib.dsl;     <!-- Library functions...                                  -->
&dbcommon.dsl;  <!-- Common ("stock") stylesheet functions                 -->
&dbctable.dsl;  <!-- Common stylesheet functions for tables                -->

&cmndbl10n.dsl; <!-- Common localization -->
&dbl10n.dsl;    <!-- Stylesheet-local localization -->

&dbadmon.dsl;   <!-- Admonitions                                           -->
&dbautoc.dsl;   <!-- Automatic TOC generation                              -->
&dbbibl.dsl;    <!-- Bibliographies                                        -->
&dbblock.dsl;   <!-- Miscellaneous block elements                          -->
&dbcallou.dsl;  <!-- Callouts                                              -->
&dbcompon.dsl;  <!-- Components; chapter-level elements                    -->
&dbdivis.dsl;   <!-- Divisions; Sets, Books, Articles, Parts               -->
&dbfootn.dsl;   <!-- Footnotes                                             -->
&dbgloss.dsl;   <!-- Glossaries                                            -->
&dbgraph.dsl;   <!-- Graphics                                              -->
&dbhtml.dsl;    <!-- HTML specific things                                  -->
&dbindex.dsl;   <!-- Indexes                                               -->
&dbinfo.dsl;    <!-- Infopools (SetInfo, BookInfo, Sect1Info, etc.)        -->
&dbinline.dsl;  <!-- Inline elements                                       -->
&dblink.dsl;    <!-- Links                                                 -->
&dblists.dsl;   <!-- Lists                                                 -->
&dblot.dsl;     <!-- Lists of Tables (ToC, LoT, etc.)                      -->
&dbmath.dsl;    <!-- Math (Equations)                                      -->
&dbmsgset.dsl;  <!-- MsgSet                                                -->
&dbnavig.dsl;
&dbparam.dsl;   <!-- General parameters                                    -->
&dbprocdr.dsl;  <!-- Procedures                                            -->
&dbrfntry.dsl;  <!-- References and RefEntrys                              -->
&dbsect.dsl;    <!-- Sections                                              -->
&dbsynop.dsl;   <!-- Synopsis                                              -->
&dbtable.dsl;   <!-- Tables                                                -->
&dbtitle.dsl;   <!-- Titles                                                -->
&dbttlpg.dsl;   <!-- Title Page                                            -->
&dbverb.dsl;    <!-- Verbatim (ProgramListing, LiteralLayout, etc.)        -->
&version.dsl;   <!-- Version -->

</style-specification-body>
</style-specification>

<external-specification id="bmno" document="dbl1bmno">
<external-specification id="dege" document="dbl1dege">
<external-specification id="dk"   document="dbl1dk">
<external-specification id="es"   document="dbl1es">
<external-specification id="fr"   document="dbl1fr">
<external-specification id="it"   document="dbl1it">
<external-specification id="nl"   document="dbl1nl">
<external-specification id="pl"   document="dbl1pl">
<external-specification id="pt"   document="dbl1pt">
<external-specification id="ru"   document="dbl1ru">
<external-specification id="svse" document="dbl1svse">
<external-specification id="usen" document="dbl1usen">

</style-sheet>


