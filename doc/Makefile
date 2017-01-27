
all: bindlib.pdf

bindlib.pdf: bindlib.tex .build/lambda.opt .build/pred2.opt
	rubber -W all --pdf $<

filter: filter.ml
	ocamlopt -o $@ $<

.build/lambda.ml .build/pred2.ml: filter bindlib.tex
	if [ ! -d .build ]; then mkdir .build; fi
	./filter bindlib.tex

.build/lambda.opt: .build/lambda.ml
	ocamlopt -o $@ -I .. bindlib.cmxa $<

.build/pred2.opt: .build/pred2.ml
	ocamlopt -o $@ -I .. bindlib.cmxa $<

clean:
	- rm -f *~ *.o *.cmx *.cmi *.cmo *.aux *.log *.toc filter
	- rm -rf .build
