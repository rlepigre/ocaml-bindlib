LIBDIR=$(shell ocamlc -where)
OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt -g

all: bindlib.cma bindlib.cmxa

## Ptmap module
ptmap.cmi: ptmap.mli
	$(OCAMLC) -c $<

ptmap.cmo: ptmap.ml ptmap.cmi
	$(OCAMLC) -c $<

ptmap.cmx: ptmap.ml ptmap.cmi
	$(OCAMLOPT) -c $<

## Util

bindlib_util.cmi: bindlib_util.mli ptmap.cmi
	$(OCAMLC) -c $<

bindlib_util.cmo: bindlib_util.ml bindlib_util.cmi
	$(OCAMLC) -c $<

bindlib_util.cmx: bindlib_util.ml bindlib_util.cmi
	$(OCAMLOPT) -c $<

## Bindlib
bindlib.cmi: bindlib.mli bindlib_util.cmi
	$(OCAMLC) -c $<

bindlib.cmo: bindlib.ml bindlib.cmi bindlib_util.cmi
	$(OCAMLC) -c $<

bindlib.cmx: bindlib.ml bindlib.cmi bindlib_util.cmi
	$(OCAMLOPT) -c $<

bindlib.cma: ptmap.cmo bindlib_util.cmo bindlib.cmo
	$(OCAMLC) -o $@ -a $^

bindlib.cmxa: ptmap.cmx bindlib_util.cmx bindlib.cmx
	$(OCAMLOPT) -o $@ -a $^

## Install
uninstall:
	rm -rf $(LIBDIR)/bindlib

install: all uninstall
	install -m 755 -d $(LIBDIR)/bindlib
	install -m 644 -p *.cmi *.cmo *.cmx *.mli *.o $(LIBDIR)/bindlib
	install -m 644 -p *.a *.cma *.cmxa META $(LIBDIR)/bindlib

## Clean
clean:
	rm -rf *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
