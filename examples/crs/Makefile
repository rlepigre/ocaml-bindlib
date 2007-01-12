CAMLC = ocamlc -pp ../../camlp4bop -I ../..
CAMLO = ocamlopt -pp ../../camlp4bop -I ../..
CAMLDEP = ocamldep

FILES = genlex.ml basic.ml print.ml globals.ml reduce.ml \
	action.ml file.ml parser.ml

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
	$(CAMLDEP) -pp ../../camlp4bop *.ml *.mli > depend

include depend

.cmo.slow:
	$(CAMLC) bindlib.cma $(OFILES) $< -o $@

.cmx.opt:
	$(CAMLO) bindlib.cmxa $(XFILES) $< -o $@

.ml.cmo:
	$(CAMLC) $(ZFLAGS) -c $<

.ml.cmx: 
	$(CAMLO) $(ZFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(ZFLAGS) -c $<

clean:
	- rm -f *.cmo *.cmx *.cmi *.o crs.slow crs.opt 



