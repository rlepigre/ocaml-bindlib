all: lambda pred2 unif translate hash_lambda make_pts make_crs

make_pts:
	make -C pts

make_crs:
	make -C crs

## Lambda calculus
lambda: lambda.ml
	ocamlfind ocamlc -I .. bindlib.cma -g -o $@ $<

## Hash consed lambda calculus, need decap
hash_lambda: hash_lambda.ml
	- ocamlfind ocamlc -pp pa_ocaml -I .. bindlib.cma -package decap -linkpkg -o $@ $<

## Second-order predicate calculus
pred2: pred2.ml
	ocamlfind ocamlc -I .. bindlib.cma -g -o $@ $<

## Higher-order unification
unif: unif.ml
	ocamlfind ocamlc -I .. bindlib.cma -g -o $@ $<

## Translation without any environment
translate: translate.ml
	ocamlfind ocamlc -I .. bindlib.cma -g -o $@ $<

## Clean
clean:
	- rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a
	make -C pts clean
	make -C crs clean

distclean: clean
	- rm -f lambda pred2 unif translate hash_lambda
