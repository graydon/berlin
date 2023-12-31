;; $Id: dbparam.dsl,v 1.1 1999/08/20 09:13:56 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/docbook/
;;

;; === Book intro, for dsl2man ==========================================

<![CDATA[
;; DOCINFO
;; <title>DocBook HTML Parameters</title>
;; <subtitle>Part of the Modular DocBook Stylesheet distribution</subtitle>
;; <author><firstname>Norman</firstname><surname>Walsh</surname>
;; </author>
;; <edition>1.14</edition>
;; <copyright><year>1997</year><year>1998</year>
;; <holder>Norman Walsh</holder></copyright>
;; <legalnotice>
;; <para>
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL NORMAN WALSH OR ANY OTHER
;; CONTRIBUTOR BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;; </para>
;; </legalnotice>
;; <legalnotice>
;; <para>
;; Please direct all questions, bug reports, or suggestions for changes
;; to Norman Walsh, &lt;<literal>ndw@nwalsh.com</literal>&gt;.
;; </para>
;; <para>
;; See <ulink url="http://nwalsh.com/docbook/dsssl/">http://nwalsh.com/docbook/dsssl/</ulink> for more information.</para>
;; </legalnotice>
;; /DOCINFO
]]>

;; ------------------------------ Parameters ------------------------------

(define %refentry-generate-name% 
  ;; REFENTRY htp-refentry-generate-name
  ;; PURP Output NAME header before 'RefName'(s)?
  ;; DESC
  ;; If true, a "NAME" section title is output before the list
  ;; of 'RefName's.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %refentry-function% 
  ;; REFENTRY htp-refentry-function
  ;; PURP Are 'RefEntry's functions?
  ;; DESC
  ;; If true, 'RefEntry's are assumed to describe functions.
  ;; If 'RefEntry's are functions, "'()'"
  ;; is output after each 'RefName'
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %funcsynopsis-style% 
  ;; REFENTRY htp-funcsynopsis-style
  ;; PURP What style of 'FuncSynopsis' should be generated?
  ;; DESC
  ;; If '%funcsynopsis-style%' is 'ansi',
  ;; ANSI-style function synopses are generated for a 'FuncSynopsis',
  ;; otherwise K<![CDATA[&]]>R-style function synopses are generated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'ansi)

(define %funcsynopsis-decoration%
  ;; REFENTRY htp-funcsynopsis-decoration
  ;; PURP Decorate elements of a FuncSynopsis?
  ;; DESC
  ;; If true, elements of the FuncSynopsis will be decorated (e.g. bold or
  ;; italic).  The decoration is controlled by functions that can be redefined
  ;; in a customization layer.  See 'edbsynop.dsl'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %default-simplesect-level% 
  ;; REFENTRY htp-default-simplesect-level
  ;; PURP Default section level for 'SimpleSect's.
  ;; DESC
  ;; If 'SimpleSect's appear inside other section-level
  ;; elements, they are rendered at the appropriate section level, but if they
  ;; appear in a component-level element, they are rendered at 
  ;; '%default-simplesect-level%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  4)

(define %generate-set-titlepage%
  ;; REFENTRY htp-generate-set-titlepage
  ;; PURP Should a set title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Set'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-book-titlepage%
  ;; REFENTRY htp-generate-book-titlepage
  ;; PURP Should a book title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Book'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-part-titlepage% 
  ;; REFENTRY htp-generate-part-titlepage
  ;; PURP Should a part title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Part'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-article-titlepage% 
  ;; REFENTRY htp-generate-article-titlepage
  ;; PURP Should an article title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Article'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-reference-titlepage% 
  ;; REFENTRY htp-generate-reference-titlepage
  ;; PURP Should a reference title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Reference'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %titlepage-in-info-order% 
  ;; REFENTRY htp-titlepage-in-info-order
  ;; PURP Place elements on title page in document order?
  ;; DESC
  ;; If true, the elements on the title page will be set in the order that
  ;; they appear in the *info element.  Otherwise, they will be set in
  ;; the order specified in the *-titlepage-*-elements list.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-set-toc% 
  ;; REFENTRY htp-generate-set-toc
  ;; PURP Should a Table of Contents be produced for Sets?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Set'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-book-toc% 
  ;; REFENTRY htp-generate-book-toc
  ;; PURP Should a Table of Contents be produced for Books?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Book'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-part-toc% 
  ;; REFENTRY htp-generate-part-toc
  ;; PURP Should a Table of Contents be produced for Parts?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Part'.
  ;; Note: '%generate-part-toc-on-titlepage%' controls whether the Part TOC
  ;; is placed on the bottom of the part titlepage or on page(s) of its own.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-reference-toc% 
  ;; REFENTRY htp-generate-reference-toc
  ;; PURP Should a Table of Contents be produced for References?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Reference'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define $generate-chapter-toc$ 
  ;; REFENTRY htp-generate-chapter-toc
  ;; PURP Should a Chapter Table of Contents be produced?
  ;; DESC
  ;; If true, an automatically generated
  ;; chapter TOC should be included. By default, it's true.  It's false if
  ;; the output is going to a single file and the current node isn't the
  ;; root element.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (lambda ()
    (or (not nochunks)
	(node-list=? (current-node) (sgml-root-element)))))

(define %generate-part-toc-on-titlepage%
  ;; REFENTRY htp-generate-part-toc-on-titlepage
  ;; PURP Should the Part TOC appear on the Part title page?
  ;; DESC
  ;; If true, the Part TOC will be placed on the Part title page.  If false,
  ;; the TOC will be placed on separate page(s) after the Part title page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-reference-toc-on-titlepage%
  ;; REFENTRY htp-generate-reference-toc-on-titlepage
  ;; PURP Should the Reference TOC appear on the Reference title page?
  ;; DESC
  ;; If true, the Reference TOC will be placed on the Reference title page.
  ;; If false,
  ;; the TOC will be placed after the Reference title page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-partintro-on-titlepage%
  ;; REFENTRY htp-generate-partintro-on-titlepage
  ;; PURP Should the PartIntro appear on the Part/Reference title page?
  ;; DESC
  ;; If true, the PartIntro content will appear on the title page of
  ;; Parts and References.  If false,
  ;; it will be placed on separate page(s) after the title page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-article-toc% 
  ;; REFENTRY htp-generate-article-toc
  ;; PURP Should a Table of Contents be produced for Articles?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Article'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define ($generate-book-lot-list$)
  ;; REFENTRY htp-generate-book-lot-list
  ;; PURP Which Lists of Titles should be produced for Books?
  ;; DESC
  ;; This parameter should be a list (possibly empty) of the elements
  ;; for which Lists of Titles should be produced for each 'Book'.
  ;;
  ;; It is meaningless to put elements that do not have titles in this
  ;; list.  If elements with optional titles are placed in this list, only
  ;; the instances of those elements that do have titles will appear in
  ;; the LOT.
  ;;
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (list (normalize "table")
	(normalize "figure")
	(normalize "example")
	(normalize "equation")))

(define %force-chapter-toc% 
  ;; REFENTRY htp-force-chapter-toc
  ;; PURP Force a chapter TOC even if it includes only a single entry
  ;; DESC
  ;; Force chapter toc indicates whether or not an automatically generated
  ;; chapter TOC should be included even if it has only one entry.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %annotate-toc%
  ;; REFENTRY htp-annotate-toc
  ;; PURP Annotate TOC entries
  ;; DESC
  ;; If #t, TOC entries will be annotated (e.g., the RefPurpose
  ;; of a RefEntry will be displayed in the TOC).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-legalnotice-link%
  ;; REFENTRY htp-generate-legalnotice-link
  ;; PURP Should legal notices be a link to a separate file?
  ;; DESC
  ;; If true, legal notices will be references to a separate file.
  ;; Note: the support for this handles the case where a single *INFO
  ;; node contains several distinct legal notices, but won't
  ;; handle multiple legal notices in different *INFO nodes.
  ;; (Each set will overwrite the previous.)  A more complex
  ;; approach could be implemented, but this is sufficient for
  ;; the current demand.  Let me know...
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define ($legalnotice-link-file$ legalnotice)
  ;; REFENTRY htp-legalnotice-link-file
  ;; PURP Name of output file for legal notices
  ;; DESC
  ;; Name of the output file for legal notices if 
  ;; '%generate-legalnotice-link%' is true.  Since several legal notices
  ;; may occur (in a Set of Books, for example), this is no longer a fixed
  ;; filename.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (string-append "ln" 
		 (number->string (all-element-number legalnotice))
		 %html-ext%))

(define %writing-mode%
  ;; REFENTRY htp-writing-mode
  ;; PURP The writing mode
  ;; DESC
  ;; The writing mode is either 'left-to-right', or 
  ;; 'right-to-left'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'left-to-right)

(define %html-ext% 
  ;; REFENTRY htp-html-ext
  ;; PURP Default extension for HTML output files
  ;; DESC
  ;; The default extension for HTML output files.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".html")

(define %use-id-as-filename%
  ;; REFENTRY htp-use-id-as-filename
  ;; PURP Use ID attributes as name for component HTML files?
  ;; DESC
  ;; If '%use-id-as-filename%' is true, the stylesheet will use the 
  ;; value of the ID attribute on a component as the base filename instead
  ;; of using the auto-generated base.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %root-filename%
  ;; REFENTRY htp-root-filename
  ;; PURP Name for the root HTML document
  ;; DESC
  ;; The filename of the root HTML document (e.g, "index").
  ;; If '#f', then a default name will be selected based on the element
  ;; type of the root element (e.g, book1.htm, set1.htm, c1.htm, etc.).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %section-autolabel%
  ;; REFENTRY htp-section-autolabel
  ;; PURP Are sections enumerated?
  ;; DESC
  ;; If true, unlabeled sections will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %chapter-autolabel% 
  ;; REFENTRY htp-chapter-autolabel
  ;; PURP Are chapters enumerated?
  ;; DESC
  ;; If true, chapters will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; Should line numbers be printed on linespecific things?
;;
(define %number-synopsis-lines% 
  ;; REFENTRY htp-number-synopsis-lines
  ;; PURP Enumerate lines in a 'Synopsis'?
  ;; DESC
  ;; If true, lines in each 'Synopsis' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%', 
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-funcsynopsisinfo-lines% 
  ;; REFENTRY htp-number-funcsynopsisinfo-lines
  ;; PURP Enumerate lines in a 'FuncSynopsisInfo'?
  ;; DESC
  ;; If true, lines in each 'FuncSynopsisInfo' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-literallayout-lines% 
  ;; REFENTRY htp-number-literallayout-lines
  ;; PURP Enumerate lines in a 'LiteralLayout'?
  ;; DESC
  ;; If true, lines in each 'LiteralLayout' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-address-lines% 
  ;; REFENTRY htp-number-address-lines
  ;; PURP Enumerate lines in a 'Address'?
  ;; DESC
  ;; If true, lines in each 'Address' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-programlisting-lines%
  ;; REFENTRY htp-number-programlisting-lines
  ;; PURP Enumerate lines in a 'ProgramListing'?
  ;; DESC
  ;; If true, lines in each 'ProgramListing' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-screen-lines%
  ;; REFENTRY htp-number-screen-lines
  ;; PURP Enumerate lines in a 'Screen'?
  ;; DESC
  ;; If true, lines in each 'Screen' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %linenumber-mod% 
  ;; REFENTRY htp-linenumber-mod
  ;; PURP Controls line-number frequency in enumerated environments.
  ;; DESC
  ;; Every '%linenumber-mod%' line will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  5)

(define %linenumber-length% 
  ;; REFENTRY htp-linenumber-length
  ;; PURP Width of line numbers in enumerated environments
  ;; DESC
  ;; Line numbers will be padded to '%linenumber-length%'
  ;; characters.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  3)

(define %linenumber-padchar% 
  ;; REFENTRY htp-linenumber-padchar
  ;; PURP Pad character in line numbers
  ;; DESC
  ;; Line numbers will be padded (on the left) with '%linenumber-padchar%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  " ")

(define ($linenumber-space$) 
  ;; REFENTRY htp-linenumber-space
  ;; PURP Returns the sosofo which separates line numbers from the text
  ;; DESC
  ;; The sosofo returned by '($linenumber-space$)' is placed
  ;; between the line number and the corresponding line in 
  ;; enumerated environments.
  ;;
  ;; Note: '%linenumber-padchar%'s are separated from lines
  ;; that are not enumerated (because they don't match '%linenumber-mod%').
  ;; In other words, '($linenumber-space$)' occurs
  ;; on every line.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (make entity-ref name: "nbsp"))

(define %callout-default-col% 
  ;; REFENTRY htp-callout-default-col
  ;; PURP Default column for callouts
  ;; DESC
  ;; If the coordinates of a callout include only a line number, the callout
  ;; bug will appear in column '%callout-default-col%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  60)

(define %simplelist-column-width% 
  ;; REFENTRY htp-simplelist-column-width
  ;; PURP Width of columns in tabular simple lists
  ;; DESC
  ;; If SimpleLists are presented in a table, how wide should the table
  ;; columns be?  If '#f', no width will be specified.
  ;;
  ;; If not #f, this value should be a string (it will be used in the WIDTH
  ;; attribute on the TD for each table entry).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %graphic-default-extension% 
  ;; REFENTRY htp-graphic-default-extension
  ;; PURP Default extension for graphic FILEREFs
  ;; DESC
  ;; The '%graphic-default-extension%' will be
  ;; added to the end of all 'fileref' filenames on
  ;; 'Graphic's if they do not end in one of the
  ;; '%graphic-extensions%'.  Set this to '#f'
  ;; to turn off this feature.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %graphic-extensions% 
  ;; REFENTRY htp-graphic-extensions
  ;; PURP List of graphic filename extensions
  ;; DESC
  ;; The list of extensions which may appear on a 'fileref'
  ;; on a 'Graphic' which are indicative of graphic formats.
  ;;
  ;; Filenames that end in one of these extensions will not have
  ;; the '%graphic-default-extension%' added to them.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '("gif" "jpg" "jpeg" "tif" "tiff" "eps" "epsf" ))

(define nochunks
  ;; REFENTRY htp-nochunks
  ;; PURP Suppress chunking of output pages
  ;; DESC
  ;; If true, the entire source document is formatted as a single HTML
  ;; document and output on stdout.
  ;; (This option can conveniently be set with '-V nochunks' on the 
  ;; Jade command line).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %author-othername-in-middle%
  ;; REFENTRY htp-othername-in-middle
  ;; PURP Author OTHERNAME appears between FIRSTNAME and SURNAME?
  ;; DESC
  ;; If true, the OTHERNAME of an AUTHOR appears between the 
  ;; FIRSTNAME and SURNAME.  Otherwise, OTHERNAME is suppressed.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define biblio-citation-check
  ;; REFENTRY htp-biblio-citation-check
  ;; PURP Check citations
  ;; DESC
  ;; If true, the content of CITATIONs will be checked against possible
  ;; biblioentries.  If the citation cannot be found, an error is issued
  ;; and the citation is generated.  If the citation is found, it is generated
  ;; with a cross reference to the appropriate biblioentry.
  ;;
  ;; A citation matches if the content of the citation element matches the
  ;; ID, XREFLABEL, or leading ABBREV of a biblioentry.
  ;;
  ;; This setting may have significant performance implications on large
  ;; documents, hence it is false by default.
  ;;
  ;; (This option can conveniently be set with '-V biblio-citation-check' 
  ;; on the Jade command line).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define biblio-filter-used
  ;; REFENTRY htp-filter-used
  ;; PURP Suppress unreferenced bibliography entries
  ;; DESC
  ;; If true, bibliography entries which are not cited are suppressed.
  ;; A biblioentry is cited if an XREF or LINK matches its ID, or if
  ;; a CITE element matches its
  ;; ID, XREFLABEL, or leading ABBREV.
  ;;
  ;; A BIBLIOGRAPHY with no entries will still be output (making a whole
  ;; component conditional would be _A LOT_ of work and seems unnecessary),
  ;; but BIBLIDIVs with no entries will be suppressed.
  ;;
  ;; This setting may have significant performance implications,
  ;; hence it is false by default.
  ;;
  ;; (This option can conveniently be set with '-V biblio-filter-used' on the 
  ;; Jade command line).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define biblio-number
  ;; REFENTRY htp-biblio-number
  ;; PURP Enumerate bibliography entries
  ;; DESC
  ;; If true, bibliography entries will be numbered.  If you cross-reference
  ;; bibliography entries, you should probably use biblio-number or
  ;; consistently use XREFLABEL or ABBREV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %olink-outline-ext%
  ;; REFENTRY htp-olink-outline-ext
  ;; PURP Extension for olink outline file
  ;; DESC
  ;; The extension used to find the outline information file.  When searching
  ;; for outline information about a document, the extension is discarded
  ;; from the system ID of the file and '%olinke-outline-ext%' is appended.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".olink")

(define %olink-resolution% 
  ;; REFENTRY htp-olink-resolution
  ;; PURP URL script for OLink resolution
  ;; DESC
  ;; OLink resolution requires a server component, '%olink-resolution%'
  ;; identifies that component.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "/cgi-bin/olink?")

(define %olink-pubid% 
  ;; REFENTRY htp-olink-pubid
  ;; PURP Portion of the URL which identifies the public identifier
  ;; DESC
  ;; Portion of the URL which identifies the public identifier
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "pubid=")

(define %olink-sysid% 
  ;; REFENTRY htp-olink-sysid
  ;; PURP Portion of the URL which identifies the system identifier
  ;; DESC
  ;; Portion of the URL which identifies the system identifier.  System
  ;; identifiers are only used if no public identifier is provided.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "sysid=")

(define %olink-fragid%
  ;; REFENTRY htp-olink-fragid
  ;; PURP Portion of the URL which identifies the fragment identifier
  ;; DESC
  ;; Portion of the URL which identifies the fragment identifier
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "&#38;fragid=")

(define ($table-element-list$) 
  ;; REFENTRY htp-table-element-list
  ;; PURP List of table element names
  ;; DESC
  ;; The list of table elements in the DTD.  
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (list (normalize "table") (normalize "informaltable")))

(define %footnotes-at-end%
  ;; REFENTRY htp-footnotes-at-end
  ;; PURP Should footnotes appear at the end of HTML pages?
  ;; DESC
  ;; If '#t', footnotes will be placed at the end of each HTML page
  ;; instead of immediately following the place where they occur.
  ;; Note: support for this feature is dependent on the processing
  ;; performed by the (footer-navigation) function; if you replace
  ;; that function, make sure that you're replacement calls
  ;; (make-endnotes).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define ($object-titles-after$)
  ;; REFENTRY htp-object-titles-after
  ;; PURP List of objects who's titles go after the object
  ;; DESC
  ;; Titles of formal objects (Figures, Equations, Tables, etc.)
  ;; in this list will be placed below the object instead of above it.
  ;; For example, 
  ;; '(define ($object-titles-after$) (list (normalize "figure") (normalize "table")))'
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '())

(define %default-title-end-punct% 
  ;; REFENTRY htp-default-title-end-punct
  ;; PURP Default punctuation at the end of a run-in head.
  ;; DESC
  ;; The punctuation used at the end of a run-in head (e.g. on FORMALPARA).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".")

(define %content-title-end-punct% 
  ;; REFENTRY htp-content-title-end-punct
  ;; PURP List of punctuation chars at the end of a run-in head
  ;; DESC
  ;; If a run-in head ends in any of these characters, the
  ;; '%default-title-end-punct%' is not used.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '(#\. #\! #\?))

(define html-index
  ;; REFENTRY htp-html-index
  ;; PURP HTML indexing?
  ;; DESC
  ;; Turns on HTML indexing.  If true, then index data will be written
  ;; to the file defined by 'html-index-filename'.  This data can be
  ;; collated and turned into a DocBook index with bin/collateindex.pl.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define html-index-filename
  ;; REFENTRY htp-html-index-filename
  ;; PURP Name of HTML index file
  ;; DESC
  ;; The name of the file to which index data will be written if
  ;; 'html-index' is not '#f'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "HTML.index")

(define html-manifest
  ;; REFENTRY htp-html-manifest
  ;; PURP Write a manifest?
  ;; DESC
  ;; If not '#f' then the list of HTML files created by the stylesheet
  ;; will be written to the file named by 'html-manifest-filename'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define html-manifest-filename
  ;; REFENTRY htp-html-manifest-filename
  ;; PURP Name of HTML manifest file
  ;; DESC
  ;; The name of the file to which a manifest will be written if
  ;; 'html-manifest' is not '#f'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "HTML.manifest")

(define image-library
  ;; REFENTRY htp-image-library
  ;; PURP Load image library database for additional info about images?
  ;; DESC
  ;; If true, an image library database is loaded and extra information
  ;; about web graphics is retrieved from it.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define image-library-filename
  ;; REFENTRY htp-image-library-filename
  ;; PURP Name of the image library database
  ;; DESC
  ;; If 'image-library' is true, then the database is loaded from
  ;; 'image-library-filename'.  It's a current limitation that only a
  ;; single database can be loaded.
  ;; 
  ;; The image library database is stored in a separate directory
  ;; because it must be parsed with the XML declaration.  The only
  ;; practical way to accomplish this with Jade, if you are processing a
  ;; document that uses another declaration, is by having a catalog
  ;; file in the directory that contains the image library that
  ;; specifies the SGMLDECL.  (So if it was in the same directory
  ;; as your document, your document would also be parsed with the
  ;; XML declaration, which may not be correct.)
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "imagelib/imagelib.xml")

(define %html-pubid%
  ;; REFENTRY htp-html-pubid
  ;; PURP What public ID are you declaring your HTML compliant with?
  ;; DESC
  ;; The public ID used in output HTML files.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "-//Norman Walsh//DTD DocBook HTML 1.0//EN")

(define %link-mailto-url%
  ;; REFENTRY htp-link-mailto-url
  ;; PURP Mailto URL for LINK REL=made
  ;; DESC
  ;; If not '#f', the '%link-mailto-url%' address will be used in a 
  ;; LINK REL=made element in the HTML HEAD.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %html-header-tags% 
  ;; REFENTRY htp-html-header-tags
  ;; PURP What additional HEAD tags should be generated?
  ;; DESC
  ;; A list of the the HTML HEAD tags that should be generated.
  ;; The format is a list of lists, each interior list consists
  ;; of a tag name and a set of attribute/value pairs:
  ;; '(("META" ("NAME" "name") ("CONTENT" "content")))
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '())

(define %stylesheet%
  ;; REFENTRY htp-stylesheet
  ;; PURP Name of the stylesheet to use
  ;; DESC
  ;; The name of the stylesheet to place in the HTML LINK TAG, or '#f' to
  ;; suppress the stylesheet LINK.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %stylesheet-type%
  ;; REFENTRY htp-stylesheet-type
  ;; PURP The type of the stylesheet to use
  ;; DESC
  ;; The type of the stylesheet to place in the HTML LINK TAG.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "text/css")

(define %css-decoration%
  ;; REFENTRY htp-css-decoration
  ;; PURP Enable CSS decoration of elements
  ;; DESC
  ;; If '%css-decoration%' is turned on then HTML elements produced by the
  ;; stylesheet may be decorated with STYLE attributes.  For example, the
  ;; LI tags produced for list items may include a fragment of CSS in the
  ;; STYLE attribute which sets the CSS property "list-style-type".
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)
  
(define %css-liststyle-alist%
  ;; REFENTRY htp-css-liststyle-alist
  ;; PURP Map DocBook OVERRIDE and MARK attributes to CSS
  ;; DESC
  ;; If '%css-decoration%' is turned on then the list-style-type property of
  ;; list items will be set to reflect the list item style selected in the
  ;; DocBook instance.  This associative list maps the style type names used
  ;; in your instance to the appropriate CSS names.  If no mapping exists,
  ;; the name from the instance will be used.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '(("bullet" "disc")
    ("box" "square")))

(define %body-attr% 
  ;; REFENTRY htp-body-attr
  ;; PURP What attributes should be hung off of BODY?
  ;; DESC
  ;; A list of the the BODY attributes that should be generated.
  ;; The format is a list of lists, each interior list contains the
  ;; name and value of a BODY attribute.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (list
   (list "BGCOLOR" "#FFFFFF")
   (list "TEXT" "#000000")))

;;   With what attributes on
;; the table used to wrap the shading?
(define %shade-verbatim%  
  ;; REFENTRY htp-shade-verbatim
  ;; PURP Should verbatim environments be shaded?
  ;; DESC
  ;; If true, a table with '($shade-verbatim-attr$)' attributes will be
  ;; wrapped around each verbatim environment.  This gives the effect
  ;; of a shaded verbatim environment.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define ($shade-verbatim-attr$)
  ;; REFENTRY htp-shade-verbatim-attr
  ;; PURP Attributes used to create a shaded verbatim environment.
  ;; DESC
  ;; See '%shade-verbatim%'
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (list
   (list "BORDER" "0")
   (list "BGCOLOR" "#E0E0E0")
   (list "WIDTH" ($table-width$))))

(define %default-quadding%   
  ;; REFENTRY htp-default-quadding
  ;; PURP The default quadding
  ;; DESC
  ;; At present, this is only used on paragraphs.  It specifies the
  ;; value of the ALIGN attribute on the paragraph.  This would be better
  ;; done with CSS, but not all browsers support it yet and this has been
  ;; oft requested functionality.
  ;;
  ;; A value of #f suppresses the ALIGN attribute altogether.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %cals-table-class%
  ;; REFENTRY cals-table-class
  ;; PURP Class attribute for CALS tables
  ;; DESC
  ;; This value, if not '#f', will be used as the value of the CLASS
  ;; attribute on CALS tables.  This allows the HTML stylesheet to
  ;; distinguish between HTML tables generated from tables in the
  ;; source document from HTML tables generated for other reasons
  ;; (simplelists and navigation, for example).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "CALSTABLE")

(define ($table-width$)
  ;; REFENTRY htp-table-width
  ;; PURP Calculate table width
  ;; DESC
  ;; This function is called to calculate the width of tables that should
  ;; theoretically be "100%" wide. Unfortunately, in HTML, a 100% width 
  ;; table in a list hangs off the right side of the browser window.  (Who's
  ;; mistake was that!).  So this function provides a way to massage
  ;; the width appropriately.
  ;;
  ;; This version is fairly dumb.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (if (has-ancestor-member? (current-node) '("LISTITEM"))
      "90%"
      "100%"))

(define %admon-graphics%
  ;; REFENTRY htp-admon-graphics
  ;; PURP Use graphics in admonitions?
  ;; DESC
  ;; If true, admonitions are presented in an alternate style that uses
  ;; a graphic.  Default graphics are provided in the distribution.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %admon-graphics-path%
  ;; REFENTRY htp-admon-graphics-path
  ;; PURP Path to admonition graphics
  ;; DESC
  ;; Sets the path, probably relative to the directory where the HTML
  ;; files are created, to the admonition graphics.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "../images/")

(define ($admon-graphic$ #!optional (nd (current-node)))
  ;; REFENTRY htp-admon-graphic
  ;; PURP Admonition graphic file
  ;; DESC
  ;; Given an admonition node, returns the name of the graphic that should
  ;; be used for that admonition.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (cond ((equal? (gi nd) (normalize "tip"))
	 (string-append %admon-graphics-path% "tip.gif"))
	((equal? (gi nd) (normalize "note"))
	 (string-append %admon-graphics-path% "note.gif"))
	((equal? (gi nd) (normalize "important"))
	 (string-append %admon-graphics-path% "important.gif"))
	((equal? (gi nd) (normalize "caution"))
	 (string-append %admon-graphics-path% "caution.gif"))
	((equal? (gi nd) (normalize "warning"))
	 (string-append %admon-graphics-path% "warning.gif"))
	(else (error (string-append (gi nd) " is not an admonition.")))))

(define ($admon-graphic-width$ #!optional (nd (current-node)))
  ;; REFENTRY htp-admon-graphic-width
  ;; PURP Admonition graphic file width
  ;; DESC
  ;; Given an admonition node, returns the width of the graphic that will
  ;; be used for that admonition.
  ;;
  ;; All of the default graphics in the distribution are 25 pixels wide.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "25")

(define %spacing-paras%
  ;; REFENTRY htp-spacing-paras
  ;; PURP Block-element spacing hack
  ;; DESC
  ;; Should extraneous "P" tags be output to force the correct vertical
  ;; spacing around things like tables.  This is ugly because different
  ;; browsers do different things.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %fix-para-wrappers%
  ;; REFENTRY htp-fix-para-wrappers
  ;; PURP Block element in para hack
  ;; DESC
  ;; Block elements are allowed in PARA in DocBook, but not in P in
  ;; HTML.  With '%fix-para-wrappers%' turned on, the stylesheets attempt
  ;; to avoid putting block elements in HTML P tags by outputting
  ;; additional end/begin P pairs around them.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %gentext-nav-use-tables%
  ;; REFENTRY htp-gentext-nav-use-tables
  ;; PURP Use tables to build the navigation headers and footers?
  ;; DESC
  ;; If true, HTML TABLEs will be used to format the header and footer
  ;; navigation information.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; If so, how wide do you want them to be?
(define %gentext-nav-tblwidth% 
  ;; REFENTRY htp-gentext-nav-tblwidth
  ;; PURP If using tables for navigation, how wide should the tables be?
  ;; DESC
  ;; If tables are used for navigation (see '%gentext-nav-use-tables%'),
  ;; how wide should the tables be?
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "100%")

(define %gentext-nav-use-ff%
  ;; REFENTRY htp-gentext-nav-use-ff
  ;; PURP Add "fast-forward" to the navigation links?
  ;; DESC
  ;; Do you want "fast-forward" navigation?  Probably not is my guess.
  ;; I'm not sure this works real well yet.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

;; ----------------------------------------------------------------------
;;
;; Declare initial values; perhaps more should be done this way...
;;

(declare-initial-value writing-mode 	%writing-mode%)

