
all: unif pred pts

check: all
	./pred
	./unif
	cd pts; make check

pred: pred.ml  ../camlp4bo ../bindlib.cma
	ocamlc  -pp "../camlp4bo" -I .. -o pred bindlib.cma pred.ml

hash_lambda: hash_lambda.ml  ../camlp4bo ../bindlib.cma
	ocamlc  -pp "../camlp4bo" -I .. -o hash_lambda bindlib.cma hash_lambda.ml

unif: unif.ml  ../camlp4bo ../bindlib.cma
	ocamlc  -pp "../camlp4bo" -I .. -o unif bindlib.cma unif.ml

pts: dum
	cd pts; make slow opt

dum:

clean:
	- rm *.cmx *.cmo *.cmi *.o *~ a.out *.cma *.cmxa *.a *.so unif pred; \
	cd pts; make clean  \#*\#
