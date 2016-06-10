
all: bindlib.pdf

bindlib.pdf: bindlib.tex
	rubber --pdf $<

clean:
	- rm -f *~
	rubber --pdf --clean bindlib.tex
