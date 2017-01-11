CAMLC = ocamlfind ocamlc -pp pa_ocaml -package earley,earley.str -I ../..
CAMLO = ocamlfind ocamlopt -pp pa_ocaml -package earley,earley.str -I ../..
CAMLDEP = ocamlfind ocamldep -pp pa_ocaml -package earley,earley.str -I ../..

FILES = basic.ml print.ml globals.ml reduce.ml action.ml parser.ml

OFILES = $(FILES:.ml=.cmo)

XFILES = $(FILES:.ml=.cmx)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .o .opt .slow

slow: crs.slow depend

opt: crs.opt depend

crs.slow : $(OFILES)
crs.opt : $(XFILES)

depend:
	- rm depend;\
	$(CAMLDEP) *.ml *.mli > depend

include depend

.cmo.slow:
	$(CAMLC) bindlib.cma -linkpkg $(OFILES) $< -o $@

.cmx.opt:
	$(CAMLO) bindlib.cmxa -linkpkg $(XFILES) $< -o $@

.ml.cmo:
	$(CAMLC) $(ZFLAGS) -c $<

.ml.cmx:
	$(CAMLO) $(ZFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(ZFLAGS) -c $<

clean:
	- rm -f *.cmo *.cmx *.cmi *.o crs.slow crs.opt
