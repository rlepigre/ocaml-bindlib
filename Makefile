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

util.cmi: util.mli ptmap.cmi
	$(OCAMLC) -c $<

util.cmo: util.ml util.cmi
	$(OCAMLC) -c $<

util.cmx: util.ml util.cmi
	$(OCAMLOPT) -c $<

## Bindlib
bindlib.cmi: bindlib.mli util.cmi
	$(OCAMLC) -c $<

bindlib.cmo: bindlib.ml bindlib.cmi util.cmi
	$(OCAMLC) -c $<

bindlib.cmx: bindlib.ml bindlib.cmi util.cmi
	$(OCAMLOPT) -c $<

bindlib.cma: ptmap.cmo util.cmo bindlib.cmo
	$(OCAMLC) -o $@ -a $^

bindlib.cmxa: ptmap.cmx util.cmx bindlib.cmx
	$(OCAMLOPT) -o $@ -a $^

## Clean
clean:
	rm -rf *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
