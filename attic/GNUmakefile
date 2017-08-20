VERSION    := 4.0.4
OCAMLFIND  := ocamlfind
OCAMLBUILD := ocamlbuild -use-ocamlfind

## Compilation
all: bindlib.cma bindlib.cmxa

bindlib.cma: bindlib.mli bindlib.ml
	$(OCAMLBUILD) $@

bindlib.cmxa: bindlib.mli bindlib.ml
	$(OCAMLBUILD) $@

## Installation
uninstall:
	$(OCAMLFIND) remove bindlib

install: all uninstall
	$(OCAMLFIND) install bindlib _build/bindlib.cmx _build/bindlib.a \
		_build/bindlib.cmi _build/bindlib.ml _build/bindlib.cma \
		_build/bindlib.cmxa _build/bindlib.mli _build/bindlib.o \
		_build/bindlib.cmo META

## Cleaning
clean:
	ocamlbuild -clean
	rm -f doc/lambda.ml doc/pred2.ml
	cd doc; rubber --clean bindlib.tex

distclean: clean
	rm -f doc/bindlib.pdf
	rm -rf html

## Documentation
.PHONY: doc
doc: bindlib.docdir/index.html
bindlib.docdir/index.html: bindlib.ml bindlib.mli
	$(OCAMLBUILD) -docflag -short-functors $@

## Release

.PHONY: release
release: distclean
	git push origin
	git tag -a ocaml-imagelib_$(VERSION)
	git push origin ocaml-imagelib_$(VERSION)


doc/bindlib.pdf: doc/bindlib.tex
	cd doc; ocaml filter.ml bindlib.tex
	$(OCAMLBUILD) doc/lambda.byte
	$(OCAMLBUILD) doc/pred2.byte
	cd doc; rubber -W all --pdf bindlib.tex


## Examples
.PHONY: examples
examples: examples/lambda.byte examples/unif.byte examples/pred2.byte \
	examples/translate.byte examples/hash_lambda.byte examples/crs/crs.byte \
	examples/pts/F.byte examples/pts/CoC.byte examples/pts/CoCu.byte \
	examples/pts/lntt.byte examples/metavar/metavar.byte

%.byte:
	$(OCAMLBUILD) $@

## Benchmark
bench: bench/term.native bench/simple1.native bench/simple2.native \
	bench/simple3.native bench/simple4.native
	time ./term.native > /dev/null
	time ./simple1.native > /dev/null
	#time ./simple2.native > /dev/null # Too slow
	time ./simple3.native > /dev/null
	time ./simple4.native > /dev/null

%.native:
	$(OCAMLBUILD) $@

## Distribution and OPAM
URLSSH=lama.univ-savoie.fr:WWW/bindlib
URL=https://lama.univ-savoie.fr/~raffalli/bindlib

html/index.html: README.tmpl

.PHONY: html
html: all doc
	mkdir -p html
	sed -e s/__VERSION__/$(VERSION)/g README.tmpl > html/index.html
	cp -r _build/bindlib.docdir html/doc
	cp doc/bindlib.pdf html/bindlib.pdf

.PHONY: tests
tests: bench examples

tar:
	cd ../bindlib_tar; darcs pull; make all html clean
	cd ..; tar cvfz bindlib-$(VERSION).tar.gz --exclude=_darcs --transform "s,bindlib_tar,bindlib-$(VERSION),"  bindlib_tar

distrib: html clean tar
	scp -r html/* $(URLSSH)/
	darcs push lama.univ-savoie.fr:WWW/repos/bindlib/
	scp ../bindlib-$(VERSION).tar.gz $(URLSSH)/
	ssh lama.univ-savoie.fr "cd WWW/bindlib; ln -sf bindlib-$(VERSION).tar.gz bindlib-latest.tar.gz"

OPAMREPO=$(HOME)/Caml/opam-repository/packages/bindlib

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
