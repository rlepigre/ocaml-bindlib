VERSION   = 4.0.3
LIBDIR    = $(shell ocamlfind printconf destdir)
OCAMLFIND = ocamlfind
OCAMLC    = $(OCAMLFIND) ocamlc
OCAMLOPT  = $(OCAMLFIND) ocamlopt
INSTALL   = $(OCAMLFIND) install
REMOVE    = $(OCAMLFIND) remove

all: bindlib.cma bindlib.cmxa

bindlib.cmi: bindlib.mli
	$(OCAMLC) -c $<

bindlib.cmo: bindlib.ml bindlib.cmi
	$(OCAMLC) -c $<

bindlib.cmx: bindlib.ml bindlib.cmi
	$(OCAMLOPT) -c $<

bindlib.cma: bindlib.cmo
	$(OCAMLC) -o $@ -a $^

bindlib.cmxa: bindlib.cmx
	$(OCAMLOPT) -o $@ -a $^

## Install
uninstall:
	- $(REMOVE) bindlib

install: all uninstall
	- $(REMOVE) bindlib
	$(INSTALL) bindlib *.cmi *.cmo *.cmx *.mli *.o *.a *.cma *.cmxa META

## Clean
clean:
	- rm -rf *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a
	make -C examples clean
	make -C bench clean
	make -C doc clean

distclean: clean
	- rm README.html html/*
	make -C examples distclean

URLSSH=lama.univ-savoie.fr:WWW/bindlib
URL=https://lama.univ-savoie.fr/~raffalli/bindlib

doc: all README.html
	ocamldoc -t "Bindlib" -keep-code -html -d html bindlib.mli
	mv html/index.html html/main.html
	cp README.html html/index.html
	make -C doc bindlib.pdf

tests:
	make -C examples
	make -C bench check

tar: doc distclean
	cd ../bindlib_tar; darcs pull; make all distclean
	cd ..; tar cvfz bindlib-$(VERSION).tar.gz --exclude=_darcs --transform "s,bindlib_tar,bindlib-$(VERSION),"  bindlib_tar

distrib: distclean tar
	scp -r html/* $(URLSSH)/
	scp -r doc/bindlib.pdf $(URLSSH)/bindlib.pdf
	darcs push lama.univ-savoie.fr:WWW/repos/bindlib/
	scp ../bindlib-$(VERSION).tar.gz $(URLSSH)/
	ssh lama.univ-savoie.fr "cd WWW/bindlib; ln -sf bindlib-$(VERSION).tar.gz bindlib-latest.tar.gz"

OPAMREPO=$(HOME)/Caml/opam-repository/packages/bindlib

README.html: README.tmpl
	sed -e s/__VERSION__/$(VERSION)/g README.tmpl > README.html

opamrepo: opam distrib
	mkdir -p $(OPAMREPO)/bindlib.$(VERSION)
	cp opam $(OPAMREPO)/bindlib.$(VERSION)/opam
	cp description.txt $(OPAMREPO)/bindlib.$(VERSION)/descr
	echo -n "archive: \""  > $(OPAMREPO)/bindlib.$(VERSION)/url
	echo -n "$(URL)/bindlib-$(VERSION).tar.gz" >> $(OPAMREPO)/bindlib.$(VERSION)/url
	echo "\"" >> $(OPAMREPO)/bindlib.$(VERSION)/url
	echo -n "checksum: \"" >> $(OPAMREPO)/bindlib.$(VERSION)/url
	echo -n `md5sum ../bindlib-$(VERSION).tar.gz | cut -b -32` >> $(OPAMREPO)/bindlib.$(VERSION)/url
	echo "\"" >> $(OPAMREPO)/bindlib.$(VERSION)/url
