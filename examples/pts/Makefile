CAMLC = ocamlfind ocamlc -package unix,earley,earley.str -pp pa_ocaml -I ../..
CAMLO = ocamlfind ocamlopt -package unix,earley,earley.str -pp pa_ocaml -I ../..
CAMLDEP = ocamlfind ocamldep -package unix,earley,earley.str -pp pa_ocaml -I ../..

FILES = basic.ml globals.ml print.ml type_check.ml \
	action.ml parser.ml main.ml

VFILES = F.ml CoC.ml lntt.ml CoCu.ml

OFILES = $(FILES:.ml=.cmo)

XFILES = $(FILES:.ml=.cmx)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .o .opt .slow

slow:  depend $(VFILES:.ml=.slow)

opt:  depend $(VFILES:.ml=.opt)

F.slow : F.cmo $(OFILES)
F.opt : F.cmx $(XFILES)
CoC.slow : CoC.cmo $(OFILES)
CoC.opt : CoC.cmx $(XFILES)
CoCu.slow : CoCu.cmo $(OFILES)
CoCu.opt : CoCu.cmx $(XFILES)
lntt.slow : lntt.cmo $(OFILES)
lntt.opt : lntt.cmx $(XFILES)

check:
	./F.slow < test.F
	./CoC.slow < test.CoC

depend: $(FILES)
	- rm depend;\
	$(CAMLDEP) *.ml *.mli > depend

include depend

.ml.slow:
	$(CAMLC) -linkpkg bindlib.cma $(OFILES) $< -o $@

.ml.opt:
	$(CAMLO) -linkpkg bindlib.cmxa $(XFILES) $< -o $@

.ml.cmo:
	$(CAMLC) $(ZFLAGS) -c $<

.ml.cmx:
	$(CAMLO) $(ZFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(ZFLAGS) -c $<

clean:
	- rm -f *.cmo *.cmx *.cmi *.o $(VFILES:.ml=.slow) $(VFILES:.ml=.opt)
