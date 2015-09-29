all: lambda

## Lambda calculus
lambda: lambda.ml
	ocamlfind ocamlc -package bindlib -o $@ bindlib.cma $<

## Clean
clean:
	rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
	rm -f lambda
