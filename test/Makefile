all: lambda

lambda: lambda.ml ../bindlib.cma
	ocamlc -g -I .. -o $@ bindlib.cma $<

## Clean
clean:
	rm -rf *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
	rm -rf lambda
