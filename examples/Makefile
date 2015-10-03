all: lambda pred2

## Lambda calculus
lambda: lambda.ml
	ocamlfind ocamlc -package bindlib -o $@ bindlib.cma $<

## Second-order predicate calculus
pred2: pred2.ml
	ocamlfind ocamlc -package bindlib -o $@ bindlib.cma $<

## Clean
clean:
	rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
	rm -f lambda pred2
