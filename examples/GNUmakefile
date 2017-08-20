all: lambda

lambda: lambda.ml ../_build/bindlib.cma
	ocamlc -I ../_build -o $@ bindlib.cma $^

../_build/bindlib.cma:
	$(error "You MUST run 'make' in the repository first.")

clean:
	rm -rf *.cmi *.cmo *.cmx *.o

distclean: clean
	rm -rf lambda *~
