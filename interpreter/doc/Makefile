.PHONY: all doc test clean clean_code clean_doc clean cleanall

all: doc

doc: A5.pdf

A5.pdf: A5.tex commenters.sty homework.cls
	latexmk -pdf -synctex=1 --shell-escape A5.tex


ALLCLEANABLES := $(shell find . \( -name '*.aux'\
                             -o -name '\#*\#'\
			     -o -name '*.log'\
			     -o -name '*.bbl'\
                             -o -name '*.out'\
                             -o -name '*~'\
                             -o -name '*.pdf'\
                             -o -name '*.dvi'\
                             -o -name '*.synctex.gz'\
                             -o -name '*.blg'\
                             -o -name '*.toc'\
                             -o -name '*.lot'\
			     -o -name '*.fls'\
			     -o -name '*.rip'\
			     -o -name '*.fdb_latexmk'\
			     -o -name '*.xcp'\
			     -o -name '*.xoj'\
                             -o -name '*.lof' \) -type f  -not -path "./.git/*" -not -path "./src/*")

ALLCLEANABLES += $(shell find . \( -name 'auto'\
                                   -o -name '_minted-*' \) -type d -not -path "./.git/*" -not -path "./src/*")

CLEANABLES := $(shell find . \( -name '*.aux'\
                             -o -name '\#*\#'\
			     -o -name '*.log'\
			     -o -name '*.bbl'\
                             -o -name '*.out'\
                             -o -name '*~'\
                             -o -name '*.synctex.gz'\
                             -o -name '*.blg'\
                             -o -name '*.toc'\
                             -o -name '*.lot'\
			     -o -name '*.fls'\
			     -o -name '*.rip'\
			     -o -name '*.fdb_latexmk'\
			     -o -name '*.xcp'\
			     -o -name '*.xoj'\
                             -o -name '*.lof' \) -type f  -not -path "./.git/*" -not -path "./src/*")


clean_doc:
	@for f in $(CLEANABLES); do \
		echo "REMOVING $$f";\
		rm $$f;\
	done

clean: clean_doc clean_code

cleanall: clean_code
	@for f in $(ALLCLEANABLES); do \
		echo "REMOVING $$f";\
		rm -r $$f;\
	done

