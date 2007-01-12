
all: bindlib-3.0.pdf

bindlib.mli.tex: ../bindlib.mli
	ocamldoc -latex -o bindlib.mli.tex -noheader -notrailer -hide Bindlib ../bindlib.mli

nvbindlib.mli.tex: ../nvbindlib.mli
	ocamldoc -latex -o nvbindlib.mli.tex -noheader -notrailer -hide Bindlib ../nvbindlib.mli

bindlib-3.0.pdf: bindlib-3.0.tex bindlib.mli.tex nvbindlib.mli.tex
	pdflatex bindlib-3.0.tex; pdflatex bindlib-3.0.tex

clean:
	- rm -r *.aux *.toc *.mli.tex *.log *~ *.raux _whizzy* ._whizzy* _region* *.wdvi