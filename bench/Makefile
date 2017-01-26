# write the path to the caml-light library here
LIBDIR=/usr/pd/caml-light/csl/lib

#you should not need any modification after this line

CAMLC=ocamlc
CAMLO=ocamlopt
CAMLLINK=ocamlc
OPTION=

all: benchopt

term: ../bindlib.cma term.ml
	$(CAMLC) $(OPTION) -I .. -o term bindlib.cma term.ml

termopt: ../bindlib.cmxa term.ml
	$(CAMLO)  $(OPTION) -I .. -cclib -L.. -o termopt bindlib.cmxa term.ml

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

simple4: simple4.ml
	$(CAMLC) $(OPTION) -o simple4 simple4.ml

simple4opt: simple4.ml
	$(CAMLO) $(OPTION) -o simple4opt simple4.ml

bench: term simple1 simple2 simple3 simple4

benchopt: termopt simple1opt simple2opt simple3opt simple4opt

check: benchopt
	time ./termopt > /dev/null
	time ./simple1opt > /dev/null
	# time ./simple2opt > /dev/null too slow
	time ./simple3opt > /dev/null
	time ./simple4opt > /dev/null

clean:
	- rm *.cmx *.cmo *.cmi *.o *.cma *.cmxa *.a *~ term termopt \
	simple1 simple1opt simple2 simple2opt simple3 simple3opt nvterm nvtermopt \#*\#
