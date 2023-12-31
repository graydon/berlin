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

<!ENTITY dbl10n.dsl  SYSTEM "../common/dbl10n.dsl">

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
<!ENTITY dbgloss.dsl  SYSTEM "dbgloss.dsl">
<!ENTITY dbgraph.dsl  SYSTEM "dbgraph.dsl">
<!ENTITY dbindex.dsl  SYSTEM "dbindex.dsl">
<!ENTITY dbinfo.dsl   SYSTEM "dbinfo.dsl">
<!ENTITY dbinline.dsl SYSTEM "dbinline.dsl">
<!ENTITY dblink.dsl   SYSTEM "dblink.dsl">
<!ENTITY dblists.dsl  SYSTEM "dblists.dsl">
<!ENTITY dblot.dsl    SYSTEM "dblot.dsl">
<!ENTITY dbmath.dsl   SYSTEM "dbmath.dsl">
<!ENTITY dbmsgset.dsl SYSTEM "dbmsgset.dsl">
<!ENTITY dbprint.dsl  SYSTEM "dbprint.dsl">
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
<style-specification id="docbook" use="usen dege dk bmno ru es fr it nl pl pt svse">
<style-specification-body>

;; $Id: docbook.dsl,v 1.1 1999/08/20 09:13:57 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define if-front-page
  (external-procedure "UNREGISTERED::James Clark//Procedure::if-front-page"))

(define if-first-page
  (external-procedure "UNREGISTERED::James Clark//Procedure::if-first-page"))

(declare-characteristic heading-level 
   "UNREGISTERED::James Clark//Characteristic::heading-level" 0)

(declare-characteristic page-number-format
   "UNREGISTERED::James Clark//Characteristic::page-number-format" "1")

(declare-characteristic page-number-restart?
   "UNREGISTERED::James Clark//Characteristic::page-number-restart?" #f)

(declare-characteristic page-n-columns
   "UNREGISTERED::James Clark//Characteristic::page-n-columns" 1)

(declare-characteristic page-column-sep
   "UNREGISTERED::James Clark//Characteristic::page-column-sep" %page-column-sep%)

(declare-characteristic page-balance-columns?
   "UNREGISTERED::James Clark//Characteristic::page-balance-columns?" %page-balance-columns?%)

(define read-entity
  (external-procedure "UNREGISTERED::James Clark//Procedure::read-entity"))

(define all-element-number
  (external-procedure "UNREGISTERED::James Clark//Procedure::all-element-number"))

(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))

;; Make text that comes from unimplemented tags easy to spot
(default
  (let* ((colr-space (color-space 
		      "ISO/IEC 10179:1996//Color-Space Family::Device RGB"))
	 (red (color colr-space 1 0 0)))
    (make sequence
      color: red
      (process-children))))

&dblib.dsl;     <!-- Library functions that I don't know where else to put -->
&dbcommon.dsl;  <!-- Common ("stock") stylesheet functions                 -->
&dbctable.dsl;  <!-- Common table stylesheet functions                     -->

&dbl10n.dsl;    <!-- Localization -->

&dbadmon.dsl;   <!-- Admonitions                                           -->
&dbautoc.dsl;   <!-- Automatic TOC generation                              -->
&dbbibl.dsl;    <!-- Bibliographies                                        -->
&dbblock.dsl;   <!-- Miscellaneous block elements                          -->
&dbcallou.dsl;  <!-- Callouts                                              -->
&dbcompon.dsl;  <!-- Components; chapter-level elements                    -->
&dbdivis.dsl;   <!-- Divisions; Sets, Books, Articles, Parts               -->
&dbgloss.dsl;   <!-- Glossaries                                            -->
&dbgraph.dsl;   <!-- Graphics                                              -->
&dbindex.dsl;   <!-- Indexes                                               -->
&dbinfo.dsl;    <!-- Infopools (SetInfo, BookInfo, Sect1Info, etc.)        -->
&dbinline.dsl;  <!-- Inline elements                                       -->
&dblink.dsl;    <!-- Links                                                 -->
&dblists.dsl;   <!-- Lists                                                 -->
&dblot.dsl;     <!-- Lists of Tables (ToC, LoT, etc.)                      -->
&dbmath.dsl;    <!-- Math (Equations)                                      -->
&dbmsgset.dsl;  <!-- MsgSet                                                -->
&dbparam.dsl;   <!-- General parameters                                    -->
&dbprint.dsl;   <!-- Print macros                                          -->
&dbprocdr.dsl;  <!-- Procedures                                            -->
&dbrfntry.dsl;  <!-- References and RefEntrys                              -->
&dbsect.dsl;    <!-- Sections                                              -->
&dbsynop.dsl;   <!-- Synopsis                                              -->
&dbtable.dsl;   <!-- Tables                                                -->
&dbtitle.dsl;   <!-- Titles                                                -->
&dbttlpg.dsl;   <!-- Title Page                                            -->
&dbverb.dsl;    <!-- Verbatim (ProgramListing, LiteralLayout, etc.)        -->
&version.dsl;

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
