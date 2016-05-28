all: lambda pred2 unif

## Lambda calculus
lambda: lambda.ml
	ocamlfind ocamlc -package bindlib -o $@ bindlib.cma $<

## Second-order predicate calculus
pred2: pred2.ml
	ocamlfind ocamlc -package bindlib -g -o $@ bindlib.cma $<

## Higher-order unification
unif: unif.ml
	ocamlfind ocamlc -package bindlib -g -o $@ bindlib.cma $<

## Clean
clean:
	rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
	rm -f lambda pred2 unif
