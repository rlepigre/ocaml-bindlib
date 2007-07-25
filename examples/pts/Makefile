CAMLC = ocamlc -pp "../../camlp4bo" -I ../..
CAMLO = ocamlopt -pp "../../camlp4bo" -I ../..
CAMLDEP = ocamldep -pp "../../camlp4bo" -I ../..

FILES = genlex.ml basic.ml globals.ml print.ml type_check.ml \
	action.ml file.ml parser.ml main.ml

VFILES = F.ml CoC.ml lntt.ml

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
lntt.slow : lntt.cmo $(OFILES)
lntt.opt : lntt.cmx $(XFILES)

check:
	./F.slow < test.F
	./CoC.slow < test.Coc
	
depend: 
	- rm depend;\
	$(CAMLDEP) *.ml *.mli > depend

include depend

.ml.slow:
	$(CAMLC) bindlib.cma $(OFILES) $< -o $@

.ml.opt:
	$(CAMLO) bindlib.cmxa $(XFILES) $< -o $@

.ml.cmo:
	$(CAMLC) $(ZFLAGS) -c $<

.ml.cmx: 
	$(CAMLO) $(ZFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(ZFLAGS) -c $<

clean:
	- rm -f *.cmo *.cmx *.cmi *.o $(VFILES:.ml=.slow) $(VFILES:.ml=.opt) 



