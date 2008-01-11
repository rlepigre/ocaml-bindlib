# write the path to the caml-light library here
LIBDIR=/usr/pd/caml-light/csl/lib

#you should not need any modification after this line

CAMLC=ocamlc
CAMLO=ocamlopt
CAMLLINK=ocamlc
OPTION=

all: benchopt

term: ../bindlib.cma ../camlp4bo term.ml
	$(CAMLC)  -pp "camlp4o ../pa_bindlib.cmo" $(OPTION) -I .. -o term bindlib.cma term.ml

termopt: ../bindlib.cmxa ../camlp4bo term.ml
	$(CAMLO)  -pp "../camlp4bo" $(OPTION) -I .. -cclib -L.. -o termopt bindlib.cmxa term.ml

nvterm: ../nvbindlib.cma ../camlp4nvbo term.ml
	$(CAMLC)  -pp "../camlp4nvbo" $(OPTION) -I .. -o nvterm nvbindlib.cma nvterm.ml

nvtermopt: ../nvbindlib.cmxa ../camlp4nvbo term.ml
	$(CAMLO)  -pp "../camlp4nvbo" $(OPTION) -I .. -cclib -L.. -o nvtermopt nvbindlib.cmxa nvterm.ml

simple1: simple1.ml
	$(CAMLC) $(OPTION) -o simple1 simple1.ml

simple1opt: simple1.ml
	$(CAMLO) $(OPTION) -o simple1opt simple1.ml

simple2: simple2.ml
	$(CAMLC) $(OPTION) -o simple2 simple2.ml

simple2opt: simple2.ml
	$(CAMLO) $(OPTION) -o simple2opt simple2.ml

simple3: simple3.ml
	$(CAMLC) $(OPTION) -o simple3 simple3.ml

simple3opt: simple3.ml
	$(CAMLO) $(OPTION) -o simple3opt simple3.ml

mini: ../bindlib.cmo ../camlp4nvbo mini.ml
	$(CAMLC) -pp "camlp4nvbo" $(OPTION) -I .. -o mini ../nvbindlib.cma mini.ml

miniopt: ../bindlib.cmx ../camlp4nvbo mini.ml
	$(CAMLO)  -pp "camlp4nvbo" $(OPTION) -I .. -cclib -L.. -o miniopt ../nvbindlib.cmxa mini.ml

bench: term nvterm mini simple1 simple2 simple3

benchopt: termopt nvtermopt miniopt simple1opt simple2opt simple3opt

check: benchopt
	./termopt
	./nvtermopt
	./miniopt

clean:
	- rm *.cmx *.cmo *.cmi *.o *.cma *.cmxa *.a *~ term termopt mini miniopt \
	simple1 simple1opt simple2 simple2opt simple3 simple3opt nvterm nvtermopt \#*\#





