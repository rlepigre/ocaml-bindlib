
all: bindlib.pdf

bindlib.mli.tex: ../bindlib.mli
	ocamldoc -latex -o bindlib.mli.tex -noheader -notrailer -hide Bindlib ../bindlib.mli

nvbindlib.mli.tex: ../nvbindlib.mli
	ocamldoc -latex -o nvbindlib.mli.tex -noheader -notrailer -hide Bindlib ../nvbindlib.mli

bindlib.pdf: bindlib.tex bindlib.mli.tex nvbindlib.mli.tex
	pdflatex bindlib.tex; pdflatex bindlib.tex

clean:
	- rm -r *.aux *.toc *.mli.tex *.log *~ *.raux _whizzy* ._whizzy* _region* *.wdvi