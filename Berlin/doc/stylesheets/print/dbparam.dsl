;; $Id: dbparam.dsl,v 1.1 1999/08/20 09:13:57 gray Exp $
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; === Book intro, for dsl2man ==========================================

<![CDATA[
;; DOCINFO
;; <title>DocBook Print Parameters</title>
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

(define %visual-acuity%
  ;; REFENTRY prp-visual-acuity
  ;; PURP General measure of document text size
  ;; DESC
  ;; This parameter controls the general size of the text in the document.
  ;; Several other values (body font size and margins) have default values that
  ;; vary depending on the setting of '%visual-acuity%'. There
  ;; are three legal values: 'normal', 
  ;; the normal, standard document size (10pt body text);
  ;; 'presbyopic', 
  ;; a slightly more generous size (12pt body text); and
  ;; 'large-type',
  ;; quite large (24pt body text).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ;; "presbyopic"
  ;; "large-type"
  "normal")

(define %paper-type%
  ;; REFENTRY prp-paper-type
  ;; PURP Name of paper type
  ;; DESC
  ;; The paper type value identifies the sort of paper in use, for example, 
  ;; 'A4' or 'USletter'. Setting the paper type is an
  ;; easy shortcut for setting the correct paper height and width.
  ;; 
  ;; As distributed, only 'A4' and 'USletter' are supported.  You can add
  ;; additional paper types by updating 'page-width' and 'page-height'.
  ;; If you do, please pass along your updates. 
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ;; "A4"
  "USletter")

;; Select characteristics of the text on the printed page.  Margins,
;; line spacing, indents, etc.
;;

(define %line-spacing-factor% 
  ;; REFENTRY prp-line-spacing-factor
  ;; PURP Factor used to calculate leading
  ;; DESC
  ;; The leading is calculated by multiplying the current font size by the 
  ;; '%line-spacing-factor%'. For example, if the font size is 10pt and
  ;; the '%line-spacing-factor%' is 1.1, then the text will be
  ;; printed "10-on-11".
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1.3)

(define %head-before-factor% 
  ;; REFENTRY prp-head-before-factor
  ;; PURP Factor used to calculate space above a title
  ;; DESC
  ;; The space before a title is calculated by multiplying the font size
  ;; used in the title by the '%head-before-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.75)

(define %head-after-factor% 
  ;; REFENTRY prp-head-after-factor
  ;; PURP Factor used to calculate space below a title
  ;; DESC
  ;; The space after a title is calculated by multiplying the font size used
  ;; in the title by the '%head-after-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.5)

(define %body-start-indent% 
  ;; REFENTRY prp-body-start-indent
  ;; PURP Default indent of body text
  ;; DESC
  ;; The default indent of body text. Some elements may have more or less
  ;; indentation.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  4pi)

(define %para-sep% 
  ;; REFENTRY prp-para-sep
  ;; PURP Distance between paragraphs
  ;; DESC
  ;; The '%para-sep%' is the distance between the last line
  ;; of one paragraph and the first line of the next.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (/ %bf-size% 2.0))

(define %para-indent%
  ;; REFENTRY prp-para-indent
  ;; PURP First line start-indent for paragraphs (other than the first)
  ;; DESC
  ;; The '%para-indent%' is the amount of extra indentation that the
  ;; first line of a paragraph should receive.  This parameter applies
  ;; only to the second and subsequent paragraphs in a section.  See
  ;; '%para-indent-firstpara%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0pt)

(define %para-indent-firstpara%
  ;; REFENTRY prp-para-indent-firstpara
  ;; PURP First line start-indent for the first paragraph
  ;; DESC
  ;; The '%para-indent-firstpara%' is the amount of extra indentation
  ;; that the first line of the first paragraph of a section should receive.
  ;; This parameter is frequently '0pt' even when '%para-indent%' is 
  ;; not.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0pt)

(define %block-sep% 
  ;; REFENTRY prp-block-sep
  ;; PURP Distance between block-elements
  ;; DESC
  ;; The '%block-sep%' is the vertical distance between
  ;; block elements (figures, tables, etc.)
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (* %para-sep% 2.0))

(define %hsize-bump-factor% 
  ;; REFENTRY prp-hsize-bump-factor
  ;; PURP Font scaling factor
  ;; DESC
  ;; Internally, the stylesheet refers to font sizes in purely relative
  ;; terms. This is done by defining a scaled set of fonts 
  ;; (sizes 1, 2, 3, etc.)
  ;; based at the default text font size (e.g. 10pt). The '%hsize-bump-factor%'
  ;; describes the ratio between scaled sizes. The default is 1.2.
  ;;
  ;; Each hsize is '%hsize-bump-factor%' times larger than
  ;; the previous hsize. For example, if the base size is 10pt, and 
  ;; '%hsize-bump-factor%'
  ;; 1.2, hsize 1 is 12pt, hsize 2 is 14.4pt, hsize 3 is 17.28pt, etc.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1.2)

(define %ss-size-factor% 
  ;; REFENTRY prp-ss-size-factor
  ;; PURP Super/subscript scaling factor
  ;; DESC
  ;; When text is set as a subscript or superscript, the font size of the
  ;; text is multiplied by '%ss-size-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.6)

(define %ss-shift-factor%
  ;; REFENTRY prp-ss-shift-factor
  ;; PURP Super/subscript shift factor
  ;; DESC
  ;; When text is set as a subscript or superscript, it is set above or below
  ;; the baseline by a factor of the current font size and '%ss-shift-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.4)

(define %smaller-size-factor% 
  ;; REFENTRY prp-smaller-size-factor
  ;; PURP Smaller font scaling factor
  ;; DESC
  ;; In environments that are usually set with a slightly smaller font size,
  ;; for example block quotations, the stylesheet calculates the smaller font
  ;; size by muliplying the current font size by '%smaller-size-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.9)

(define %verbatim-size-factor% 
  ;; REFENTRY prp-verbatim-size-factor
  ;; PURP Verbatim font scaling factor
  ;; DESC
  ;; When a monospace font is selected, the current font size is multiplied
  ;; by the '%verbatim-size-factor%'. If '%verbatim-size-factor%'
  ;; is '#f', no scaling is performed (Well, that's not precisely true. 
  ;; In '$verbatim-display$'
  ;; environments, the font size is calculated with respect to the longest line
  ;; in the display, if '%verbatim-size-factor%' is '#f').
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.9)

(define %verbatim-default-width% 
  ;; REFENTRY prp-verbatim-default-width
  ;; PURP Default width of verbatim environments
  ;; DESC
  ;; If no WIDTH attribute is specified on verbatim environments, 
  ;; '%verbatim-default-width%' is the default.  Note: this width only
  ;; comes into play if '%verbatim-size-factor%' is '#f'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  80)

(define %default-quadding%   
  ;; REFENTRY prp-default-quadding
  ;; PURP The default quadding
  ;; DESC
  ;; The default quadding ('start', 'center', 'justify',
  ;; or 'end').
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %division-title-quadding% 
  ;; REFENTRY prp-division-title-quadding
  ;; PURP Division title quadding
  ;; DESC
  ;; The quadding of division-level titles ('Set', 'Book', and 'Part').
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'center)

(define %division-subtitle-quadding% 
  ;; REFENTRY prp-division-subtitle-quadding
  ;; PURP Division subtitle quadding
  ;; DESC
  ;; The quadding of division-level subtitles ('Set', 'Book', and 'Part').
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'center)

(define %component-title-quadding% 
  ;; REFENTRY prp-component-title-quadding
  ;; PURP Component title quadding
  ;; DESC
  ;; The quadding of component-level titles ('Chapter',
  ;; 'Appendix', 'Glossary', etc.).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %component-subtitle-quadding% 
  ;; REFENTRY prp-component-subtitle-quadding
  ;; PURP Component subtitle quadding
  ;; DESC
  ;; The quadding of component-level subtitles ('Chapter',
  ;; 'Appendix', 'Glossary', etc.).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %section-title-quadding% 
  ;; REFENTRY prp-section-title-quadding
  ;; PURP Section title quadding
  ;; DESC
  ;; The quadding of section-level titles.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %section-subtitle-quadding% 
  ;; REFENTRY prp-section-subtitle-quadding
  ;; PURP Section subtitle quadding
  ;; DESC
  ;; The quadding of section-level subtitles.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %title-font-family% 
  ;; REFENTRY prp-title-font-family
  ;; PURP The font family used in titles
  ;; DESC
  ;; The name of the font family used in titles (Arial by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Arial")

(define %body-font-family% 
  ;; REFENTRY prp-body-font-family
  ;; PURP The font family used in body text
  ;; DESC
  ;; The name of the font family used in body text 
  ;; (Times New Roman by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Times New Roman")

(define %mono-font-family% 
  ;; REFENTRY prp-mono-font-family
  ;; PURP The font family used in verbatim environments
  ;; DESC
  ;; The name of the font family used in verbatim environments (Courier New
  ;; by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Courier New")

(define %admon-font-family% 
  ;; REFENTRY prp-admon-font-family
  ;; PURP The font family used in admonitions
  ;; DESC
  ;; The name of the font family used for body text in admonitions (Arial
  ;; by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Arial")

(define %guilabel-font-family%
  ;; REFENTRY prp-guilabel-font-family
  ;; PURP The font family used in GUI labels
  ;; DESC
  ;; The name of the font family used for text that represents text on a
  ;; GUI (e.g., text in 'GUILabel', 'GUIMenu',
  ;; etc.). (Arial by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Arial")

(define %refentry-name-font-family%
  ;; REFENTRY prp-refentry-name-font-family
  ;; PURP The font family used in RefName
  ;; DESC
  ;; The name of the font family used in 'RefEntry'
  ;; 'RefName's.
  ;;
  ;; If '%refentry-function%' is true, defaults to
  ;; '%mono-font-family%', otherwise defaults to '%body-font-family%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (if %refentry-function%
      %mono-font-family%
      %body-font-family%))

(define %page-number-restart% 
  ;; REFENTRY prp-page-number-restart
  ;; PURP Restart page numbers in each component?
  ;; DESC
  ;; If true, page numbers are restarted at the beginning of each 
  ;; component-level
  ;; element ('Chapter', 'Appendix',
  ;; 'Bibliography', etc.).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %article-page-number-restart% 
  ;; REFENTRY prp-article-page-number-restart
  ;; PURP Restart page numbers in each article?
  ;; DESC
  ;; If true, page numbers are restarted at the beginning of each 
  ;; article.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define ($object-titles-after$)
  ;; REFENTRY prp-object-titles-after
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
  ;; REFENTRY prp-default-title-end-punct
  ;; PURP Default punctuation at the end of a run-in head.
  ;; DESC
  ;; The punctuation used at the end of a run-in head (e.g. on FORMALPARA).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".")

(define %content-title-end-punct% 
  ;; REFENTRY prp-content-title-end-punct
  ;; PURP List of punctuation chars at the end of a run-in head
  ;; DESC
  ;; If a run-in head ends in any of these characters, the
  ;; '%default-title-end-punct%' is not used.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '(#\. #\! #\?))

(define %refentry-new-page% 
  ;; REFENTRY prp-refentry-new-page
  ;; PURP 'RefEntry' starts on new page?
  ;; DESC
  ;; If true, each 'RefEntry' begins on a new page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %refentry-keep% 
  ;; REFENTRY prp-refentry-keep
  ;; PURP Keep RefEntrys together?
  ;; DESC
  ;; Refentry keep indicates how the stylesheet should
  ;; attempt to keep each RefEntry.  Common values are '#t', for the
  ;; smallest possible area, 'page' for the same page, and '#f' to ignore
  ;; this characteristic.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %refentry-generate-name% 
  ;; REFENTRY prp-refentry-generate-name
  ;; PURP Output NAME header before 'RefName'(s)?
  ;; DESC
  ;; If true, a "NAME" section title is output before the list
  ;; of 'RefName's.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %refentry-function% 
  ;; REFENTRY prp-refentry-function
  ;; PURP Are 'RefEntry's functions?
  ;; DESC
  ;; If true, 'RefEntry's are assumed to describe functions.
  ;; If 'RefEntry's are functions, "'()'"
  ;; is output after each 'RefName'
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; I don't really think I've got the ANSI/KandR styles right.  Actually,
;; I'm not sure I remember exactly what each is.  The second edition
;; of KandR uses what I thought was ANSI style...

(define %funcsynopsis-style% 
  ;; REFENTRY prp-funcsynopsis-style
  ;; PURP What style of 'FuncSynopsis' should be generated?
  ;; DESC
  ;; If '%funcsynopsis-style%' is 'ansi',
  ;; ANSI-style function synopses are generated for a 'FuncSynopsis',
  ;; otherwise K<![CDATA[&]]>R-style function synopses are generated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'ansi)

(define %kr-funcsynopsis-indent% 
  ;; REFENTRY prp-kr-funcsynopsis-indent
  ;; PURP Indent-depth in K<![CDATA[&]]>R-style function synopses
  ;; DESC
  ;; If the '%funcsynopsis-style%' is 'kr',
  ;; '%kr-funcsynopsis-indent%' specifies the amount by which parameter
  ;; definitions should be indented under the function prototype.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1pi)

(define %funcsynopsis-decoration%
  ;; REFENTRY prp-funcsynopsis-decoration
  ;; PURP Decorate elements of a FuncSynopsis?
  ;; DESC
  ;; If true, elements of the FuncSynopsis will be decorated (e.g. bold or
  ;; italic).  The decoration is controlled by functions that can be redefined
  ;; in a customization layer.  See 'edbsynop.dsl'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %default-simplesect-level% 
  ;; REFENTRY prp-default-simplesect-level
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
  ;; REFENTRY prp-generate-set-titlepage
  ;; PURP Should a set title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Set'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-book-titlepage%
  ;; REFENTRY prp-generate-book-titlepage
  ;; PURP Should a book title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Book'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-part-titlepage% 
  ;; REFENTRY prp-generate-part-titlepage
  ;; PURP Should a part title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Part'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-reference-titlepage% 
  ;; REFENTRY prp-generate-reference-titlepage
  ;; PURP Should a reference title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Reference'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-article-titlepage% 
  ;; REFENTRY prp-generate-article-titlepage
  ;; PURP Should an article title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Article'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-article-titlepage-on-separate-page%
  ;; REFENTRY prp-generate-article-ttlpg-on-sep-page
  ;; PURP Should the article title page be on a separate page?
  ;; DESC
  ;; If true, the title page for each 'Article' will occur on its own page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %titlepage-in-info-order% 
  ;; REFENTRY prp-titlepage-in-info-order
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
  ;; REFENTRY prp-generate-set-toc
  ;; PURP Should a Table of Contents be produced for Sets?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Set'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-book-toc% 
  ;; REFENTRY prp-generate-book-toc
  ;; PURP Should a Table of Contents be produced for Books?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Book'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-part-toc% 
  ;; REFENTRY prp-generate-part-toc
  ;; PURP Should a Table of Contents be produced for Parts?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Part'.
  ;; Note: '%generate-part-toc-on-titlepage%' controls whether the Part TOC
  ;; is placed on the bottom of the part titlepage or on page(s) of its own.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-reference-toc% 
  ;; REFENTRY prp-generate-reference-toc
  ;; PURP Should a Table of Contents be produced for References?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Reference'.
  ;; Note: '%generate-reference-toc-on-titlepage%' controls whether the
  ;; Reference TOC
  ;; is placed on the bottom of the title page or on page(s) of its own.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-part-toc-on-titlepage%
  ;; REFENTRY prp-generate-part-toc-on-titlepage
  ;; PURP Should the Part TOC appear on the Part title page?
  ;; DESC
  ;; If true, the Part TOC will be placed on the Part title page.  If false,
  ;; the TOC will be placed on separate page(s) after the Part title page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-reference-toc-on-titlepage%
  ;; REFENTRY prp-generate-reference-toc-on-titlepage
  ;; PURP Should the Reference TOC appear on the Reference title page?
  ;; DESC
  ;; If true, the Reference TOC will be placed on the Reference title page.
  ;; If false,
  ;; the TOC will be placed on separate page(s) after the title page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-article-toc-on-titlepage%
  ;; REFENTRY prp-generate-article-toc-on-titlepage
  ;; PURP Should the Article TOC appear on the Article title page?
  ;; DESC
  ;; If true, the Article TOC will be placed on the Article title page.
  ;; If false,
  ;; the TOC will be placed on separate page(s) after the title page.
  ;; If false, %generate-article-titlepage-on-separate-page% should be
  ;; true.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-partintro-on-titlepage%
  ;; REFENTRY prp-generate-partintro-on-titlepage
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
  ;; REFENTRY prp-generate-article-toc
  ;; PURP Should a Table of Contents be produced for Articles?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Article'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define ($generate-book-lot-list$)
  ;; REFENTRY prp-generate-book-lot-list
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

(define %two-side% 
  ;; REFENTRY prp-two-side
  ;; PURP Is two-sided output being produced?
  ;; DESC
  ;; If '%two-side%' is true, headers and footers are alternated
  ;; so that the "outer" and "inner" headers will be correctly
  ;; placed in the bound document.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %writing-mode%
  ;; REFENTRY prp-writing-mode
  ;; PURP The writing mode
  ;; DESC
  ;; The writing mode is either 'left-to-right', or 
  ;; 'right-to-left'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'left-to-right)

(define %chap-app-running-heads% 
  ;; REFENTRY prp-chap-app-running-heads
  ;; PURP Generate running headers and footers on chapter-level elements?
  ;; DESC
  ;; If true, running headers and footers are produced on chapter-level 
  ;; elements.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %section-autolabel% 
  ;; REFENTRY prp-section-autolabel
  ;; PURP Are sections enumerated?
  ;; DESC
  ;; If true, unlabeled sections will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %chapter-autolabel% 
  ;; REFENTRY prp-chapter-autolabel
  ;; PURP Are chapters enumerated?
  ;; DESC
  ;; If true, chapters will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %chap-app-running-head-autolabel% 
  ;; REFENTRY prp-chap-app-running-head-autolabel
  ;; PURP Put chapter labels in running heads?
  ;; DESC
  ;; If true, running heads on 'Chapter's and 
  ;; 'Appendix'es will include an automatic label.
  ;; 
  ;; In other words, if a 'Chapter' has no 'Label' attribute,
  ;; and '%chap-app-running-head-autolabel%'
  ;; is true, running heads will include the automatic label for the
  ;; 'Chapter'. If '%chap-app-running-head-autolabel%'
  ;; is false, only the 'Title' (or 'TitleAbbrev')
  ;; will appear in the running head.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; Should line numbers be printed on linespecific things?
;;
(define %number-synopsis-lines% 
  ;; REFENTRY prp-number-synopsis-lines
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
  ;; REFENTRY prp-number-funcsynopsisinfo-lines
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
  ;; REFENTRY prp-number-literallayout-lines
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
  ;; REFENTRY prp-number-address-lines
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
  ;; REFENTRY prp-number-programlisting-lines
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
  ;; REFENTRY prp-number-screen-lines
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
  ;; REFENTRY prp-linenumber-mod
  ;; PURP Controls line-number frequency in enumerated environments.
  ;; DESC
  ;; Every '%linenumber-mod%' line will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  5)

(define %linenumber-length% 
  ;; REFENTRY prp-linenumber-length
  ;; PURP Width of line numbers in enumerated environments
  ;; DESC
  ;; Line numbers will be padded to '%linenumber-length%'
  ;; characters.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  3)

(define %linenumber-padchar% 
  ;; REFENTRY prp-linenumber-padchar
  ;; PURP Pad character in line numbers
  ;; DESC
  ;; Line numbers will be padded (on the left) with '%linenumber-padchar%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "\no-break-space;")

(define ($linenumber-space$) 
  ;; REFENTRY prp-linenumber-space
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
  (literal "\no-break-space;"))

(define %callout-fancy-bug% 
  ;; REFENTRY prp-callout-fancy-bug
  ;; PURP Use fancy callout bugs?
  ;; DESC
  ;; If true, fancy callout bugs will be used. Otherwise, simple ones are
  ;; used. Fancy callout bugs may require the RTF backend.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %callout-default-col% 
  ;; REFENTRY prp-callout-default-col
  ;; PURP Default column for callouts
  ;; DESC
  ;; If the coordinates of a callout include only a line number, the callout
  ;; bug will appear in column '%callout-default-col%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  60)

(define %simplelist-column-width% 
  ;; REFENTRY prp-simplelist-column-width
  ;; PURP Width of columns in tabular simple lists
  ;; DESC
  ;; If set to '#f', the table will span the entire
  ;; page width.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %block-start-indent% 
  ;; REFENTRY prp-block-start-indent
  ;; PURP Extra start-indent for block-elements
  ;; DESC
  ;; Block elements (tables, figures, verbatim environments, etc.) will
  ;; be indented by the specified amount.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0pt)

(define %graphic-default-extension% 
  ;; REFENTRY prp-graphic-default-extension
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
  ;; REFENTRY prp-graphic-extensions
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
  '("eps" "epsf" "gif" "tif" "tiff" "jpg" "jpeg"))

(define %author-othername-in-middle%
  ;; REFENTRY prp-othername-in-middle
  ;; PURP Author OTHERNAME appears between FIRSTNAME and SURNAME?
  ;; DESC
  ;; If true, the OTHERNAME of an AUTHOR appears between the 
  ;; FIRSTNAME and SURNAME.  Otherwise, OTHERNAME is suppressed.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define biblio-citation-check
  ;; REFENTRY prp-biblio-citation-check
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
  ;; REFENTRY prp-filter-used
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
  ;; REFENTRY prp-biblio-number
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
  ;; REFENTRY prp-olink-outline-ext
  ;; PURP Extension for olink outline file
  ;; DESC
  ;; The extension used to find the outline information file.  When searching
  ;; for outline information about a document, the extension is discarded
  ;; from the system ID of the file and '%olinke-outline-ext%' is appended.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".olink")

(define ($table-element-list$) 
  ;; REFENTRY prp-table-element-list
  ;; PURP List of table element names
  ;; DESC
  ;; The list of table elements in the DTD.  
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (list (normalize "table") (normalize "informaltable")))

(define %admon-graphics%
  ;; REFENTRY prp-admon-graphics
  ;; PURP Use graphics in admonitions?
  ;; DESC
  ;; If true, admonitions are presented in an alternate style that uses
  ;; a graphic.  Default graphics are provided in the distribution.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %admon-graphics-path%
  ;; REFENTRY prp-admon-graphics-path
  ;; PURP Path to admonition graphics
  ;; DESC
  ;; Sets the path, probably relative to the directory where the HTML
  ;; files are created, to the admonition graphics.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "../images/")

(define ($admon-graphic$ #!optional (nd (current-node)))
  ;; REFENTRY prp-admon-graphic
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
  ;; REFENTRY prp-admon-graphic-width
  ;; PURP Admonition graphic file width
  ;; DESC
  ;; Given an admonition node, returns the width of the graphic that will
  ;; be used for that admonition.
  ;;
  ;; All of the default graphics in the distribution are 0.3in wide.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.3in)

(define image-library
  ;; REFENTRY prp-image-library
  ;; PURP Load image library database for additional info about images?
  ;; DESC
  ;; If true, an image library database is loaded and extra information
  ;; about web graphics is retrieved from it.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define image-library-filename
  ;; REFENTRY prp-image-library-filename
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

(define %footnote-ulinks%
  ;; REFENTRY prp-footnote-ulinks
  ;; PURP Generate footnotes for ULinks?
  ;; DESC
  ;; If true, the URL of each ULink will appear as a footnote.
  ;; Processing ULinks this way may be very, very slow. It requires
  ;; walking over every descendant of every component in order to count
  ;; both ulinks and footnotes.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-heading-level%   
  ;; REFENTRY prp-generate-heading-level
  ;; PURP Output RTF heading level characteristics?
  ;; DESC
  ;; If true, component and section titles will have the heading-level
  ;; characteristic in the RTF.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %page-n-columns%
  ;; REFENTRY prp-page-n-columns
  ;; PURP Sets the number of columns on each page
  ;; DESC
  ;; Sets the number of columns on each page
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1)

(define %page-column-sep%
  ;; REFENTRY prp-page-column-sep
  ;; PURP Sets the width of the gutter between columns
  ;; DESC
  ;; Sets the width of the gutter between columns
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.5in)

(define %page-balance-columns?% 
  ;; REFENTRY prp-page-balance-columns
  ;; PURP Balance columns on pages?
  ;; DESC
  ;; If true, the columns on the final page of a multiple column layout
  ;; will be balanced.  Otherwise, the columns will be completely filled in the
  ;; writing direction and the last column may be a different length 
  ;; than the preceding columns.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %left-margin% 
  ;; REFENTRY prp-left-margin
  ;; PURP Width of left margin
  ;; DESC
  ;; The '%left-margin%' parameter specifies the width of the left margin
  ;; of the page.  Note that this setting is relative to the physical page,
  ;; not the writing direction.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  6pi)

(define %right-margin% 
  ;; REFENTRY prp-right-margin
  ;; PURP Width of the right margin
  ;; DESC
  ;; The '%right-margin%' parameter specifies the width of the right margin
  ;; of the page.  Note that this setting is relative to the physical page,
  ;; not the writing direction.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  6pi)

;;
;; ----------------------------------------------------------------------
;;
;; The following parameters are generated from/depend on selections made
;; above.
;;

(define %bf-size%
  (case %visual-acuity%
	(("tiny") 8pt)
	(("normal") 10pt)
	(("presbyopic") 12pt)
	(("large-type") 24pt)))
(define-unit em %bf-size%)

(define %page-width%
  (case %paper-type%
	(("A4") 210mm)
	(("USletter") 8.5in)
	(("USlandscape") 11in)))

(define %page-height%
  (case %paper-type%
	(("A4") 297mm)
	(("USletter") 11in)
	(("USlandscape") 8.5in)))

(define %text-width% (- %page-width% (+ %left-margin% %right-margin%)))
(define %body-width% (- %text-width% %body-start-indent%))

;;
;; ----------------------------------------------------------------------
;;
;; Declare initial values; perhaps more should be done this way...
;;

(declare-initial-value writing-mode 	%writing-mode%)

(declare-initial-value input-whitespace-treatment 'collapse)

(declare-initial-value left-margin 	%left-margin%)
(declare-initial-value right-margin 	%right-margin%)
(declare-initial-value top-margin	(if (equal? %visual-acuity% "large-type") 7.5pi 6pi))
(declare-initial-value bottom-margin	(if (equal? %visual-acuity% "large-type") 9.5pi 8pi))
(declare-initial-value header-margin	(if (equal? %visual-acuity% "large-type") 5.5pi 4pi))
(declare-initial-value footer-margin	4pi)

(declare-initial-value page-width	%page-width%)
(declare-initial-value page-height	%page-height%)
