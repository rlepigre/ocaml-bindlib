all: lambda pred2 unif hash_lambda make_pts make_crs

make_pts:
	make -C pts

make_crs:
	make -C crs

## Lambda calculus
lambda: lambda.ml
	ocamlfind ocamlc -package bindlib -linkpkg -g -o $@ $<

## Hash consed lambda calculus, need decap
hash_lambda: hash_lambda.ml
	- ocamlfind ocamlc -pp pa_ocaml -package bindlib,decap -linkpkg -o $@ $<

## Second-order predicate calculus
pred2: pred2.ml
	ocamlfind ocamlc -package bindlib -linkpkg -g -o $@ $<

## Higher-order unification
unif: unif.ml
	ocamlfind ocamlc -package bindlib -linkpkg -g -o $@ $<

## Clean
clean:
	rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
	rm -f lambda pred2 unif
