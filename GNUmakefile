VERSION    := 5.0.1
OCAMLFIND  := ocamlfind
OCAMLBUILD := ocamlbuild -quiet
CFLAGS     := -cflags -w,A
DFLAGS     := -docflags -charset,utf-8,-short-functors

## Compilation
all: bindlib.cma bindlib.cmxa bindlib.cmxs META

bindlib.cma: bindlib.mli bindlib.ml
	$(OCAMLBUILD) $(CFLAGS) $@

bindlib.cmxa: bindlib.mli bindlib.ml
	$(OCAMLBUILD) $(CFLAGS) $@

bindlib.cmxs: bindlib.mli bindlib.ml
	$(OCAMLBUILD) $(CFLAGS) $@

META:
	@echo "name=\"bindlib\""                                  > $@
	@echo "version=\"$(VERSION)\""                           >> $@
	@echo "description=\"Efficient binder representation.\"" >> $@
	@echo "requires=\"\""                                    >> $@
	@echo "archive(byte)=\"bindlib.cma\""                    >> $@
	@echo "archive(native)=\"bindlib.cmxa\""                 >> $@
	@echo "plugin(byte)=\"bindlib.cma\""                     >> $@
	@echo "plugin(native)=\"bindlib.cmxs\""                  >> $@

## Examples
EXAMPLES = examples/lambda.native examples/translate.native \
           examples/basic.native examples/parsed.native \
           examples/fchurch.native

.PHONY: examples
examples: $(EXAMPLES)

examples/lambda.native: examples/lambda.ml
	$(OCAMLBUILD) $@

examples/translate.native: examples/translate.ml
	$(OCAMLBUILD) $@

examples/basic.native: examples/basic.ml
	$(OCAMLBUILD) $@

examples/parsed.native: examples/parsed.ml
	$(OCAMLBUILD) $@

examples/fchurch.native: examples/fchurch.ml
	$(OCAMLBUILD) $@

.PHONY: tests
tests: examples
	@$(foreach e,$(shell find _build -name "*.native"),$(e) &&) \
		echo "All good."

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
	find . -name "*~" -exec rm {} \;
	rm -f META

## Documentation
.PHONY: doc
doc: bindlib.docdir/index.html
bindlib.docdir/index.html: bindlib.ml bindlib.mli
	$(OCAMLBUILD) $(DFLAGS) $@

.PHONY: updatedoc
updatedoc: doc
	cp bindlib.docdir/*.* docs/ocamldoc/

## Release

.PHONY: release
release: distclean
	git push origin
	git tag -a ocaml-bindlib_$(VERSION)
	git push origin ocaml-bindlib_$(VERSION)
