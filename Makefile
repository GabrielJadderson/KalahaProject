# Makefile
#
# Converts Markdown to other formats (HTML, PDF, DOCX, RTF, ODT, EPUB) using Pandoc
# <http://johnmacfarlane.net/pandoc/>
#
# Run "make" (or "make all") to convert to all other formats
#
# Run "make clean" to delete converted files

# Convert all files in this directory that have a .lhs suffix
SOURCE_DOCS := $(wildcard *.lhs)

EXPORTED_DOCS=\
 $(SOURCE_DOCS:.lhs=.pdf) \
 # $(SOURCE_DOCS:.lhs=.html) \
 # $(SOURCE_DOCS:.lhs=.docx) \
 # $(SOURCE_DOCS:.lhs=.rtf) \
 # $(SOURCE_DOCS:.lhs=.odt) \
 # $(SOURCE_DOCS:.lhs=.epub)

RM=/bin/rm

PANDOC=pandoc

PANDOC_OPTIONS= -r markdown+lhs --smart --standalone

PANDOC_HTML_OPTIONS=--to html5
PANDOC_PDF_OPTIONS= -t latex --listings -H preamble.tex
PANDOC_DOCX_OPTIONS=
PANDOC_RTF_OPTIONS=
PANDOC_ODT_OPTIONS=
PANDOC_EPUB_OPTIONS=--to epub3


# Pattern-matching Rules

%.html : %.lhs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_HTML_OPTIONS) -o $@ $<

%.pdf : %.lhs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_PDF_OPTIONS) -o $@ $<

%.docx : %.lhs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_DOCX_OPTIONS) -o $@ $<

%.rtf : %.lhs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_RTF_OPTIONS) -o $@ $<

%.odt : %.lhs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_ODT_OPTIONS) -o $@ $<

%.epub : %.lhs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_EPUB_OPTIONS) -o $@ $<


# Targets and dependencies

.PHONY: all clean

all : $(EXPORTED_DOCS)

clean:
	- $(RM) $(EXPORTED_DOCS)
