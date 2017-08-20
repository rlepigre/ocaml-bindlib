VERSION    := 4.0.4
OCAMLFIND  := ocamlfind
OCAMLBUILD := ocamlbuild -quiet

## Compilation
all: bindlib.cma bindlib.cmxa

bindlib.cma: bindlib.mli bindlib.ml
	$(OCAMLBUILD) $@

bindlib.cmxa: bindlib.mli bindlib.ml
	$(OCAMLBUILD) $@

## Examples
.PHONY: examples
examples: examples/lambda.native

examples/lambda.native: examples/lambda.ml
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
	$(OCAMLBUILD) -clean

distclean: clean
	rm -f *~ examples/*~

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
