#
# This builds the berlin documentation in HTML and optionally PDF
# it uses files found _within_ the berlin tree to save you the hassle
# of installing all the supporting machinery yourself. You will need
# jade installed, however, as it's a little big to be distributing
# in our archive.
#

STYLESHEET = stylesheets/berlin.dsl
JADE_FLAGS = -c sgml.catalog
JADE_HTML_FLAGS = -V %use-id-as-filename% -ihtml -D ../ -d $(STYLESHEET)\#html $(JADE_FLAGS) -t sgml
JADE_TEX_FLAGS = -d $(STYLESHEET)\#print $(JADE_FLAGS) -t tex
BOOK_FILES = $(wildcard src/*.sgm)

all: html/berlin-tut.html print/berlin-tut.pdf

html/%.html: src/%.sgm $(BOOK_FILES)
	cd html; jade $(JADE_HTML_FLAGS) $<; cd ../;

print/%.tex: src/%.sgm $(BOOK_FILES)
	jade $(JADE_TEX_FLAGS) -o $@ $< 

print/%.dvi: print/%.tex
	cd print; jadetex berlin-man.tex; cd ..;

print/%.ps: print/%.dvi
	cd print; dvips -o berlin-man.ps berlin-man.dvi; cd ..;

print/%.pdf: print/%.ps
	cd print; ps2pdf berlin-man.ps; cd ..;
