all: pdf

MAINFILE  := header
RNWFILES  := 
RFILES    := 
TEXFILES  := $(wildcard introduction/*.tex) $(wildcard longHera/*.tex) $(wildcard ukb_assoc/*.tex)
CACHEDIR  := cache
#FIGUREDIR := figures
LATEXMK_FLAGS := 

##### Explicit Dependencies #####
#################################

RNWTEX = $(RNWFILES:.Rnw=.tex)
ROUTFILES = $(RFILES:.R=.Rout)
RDAFILES= $(RFILES:.R=.rda)
MAINTEX = $(MAINFILE:=.tex)
MAINPDF = $(MAINFILE:=.pdf)
ALLTEX = $(MAINTEX) $(RNWTEX) $(TEXFILES)

# Dependencies
$(RNWTEX): $(RDAFILES)
$(MAINTEX): $(RNWTEX) $(TEXFILES)
$(MAINPDF): $(MAINTEX) $(ALLTEX) 

.PHONY: pdf tex clean 

pdf: $(MAINPDF)

tex: $(RDAFILES) $(ALLTEX) 

%.tex:%.Rnw
	Rscript \
	  -e "library(knitr)" \
	  -e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
	  -e "knitr::opts_knit[['set']](root.dir = '.')" \
	  -e "knitr::opts_chunk[['set']](fig.height=4)" \
	  -e "knitr::knit('$<','$@')"

%.R:%.Rnw
	Rscript -e "Sweave('$^', driver=Rtangle())"

%.Rout:%.R
	R CMD BATCH "$^" "$@"

%.pdf: %.tex 
	latexmk -pdf $<

clean:
	-latexmk -c -quiet $(MAINFILE).tex
	-rm -f $(MAINTEX) $(RNWTEX)
	-rm -rf $(FIGUREDIR)
	-rm $(MAINPDF)

