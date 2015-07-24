all: bindlib.cma bindlib.cmxa nvbindlib.cma nvbindlib.cmxa

## Ptmap module
ptmap.cmi: ptmap.mli
	ocamlc -c $<

ptmap.cmo: ptmap.ml ptmap.cmi
	ocamlc -c $<

ptmap.cmx: ptmap.ml ptmap.cmi
	ocamlopt -c $<

## Util

util.cmi: util.mli ptmap.cmi
	ocamlc -c $<

util.cmo: util.ml util.cmi
	ocamlc -c $<

util.cmx: util.ml util.cmi
	ocamlopt -c $<

## Bindlib
bindlib.cmi: bindlib.mli util.cmi
	ocamlc -c $<

bindlib.cmo: bindlib.ml bindlib.cmi util.cmi
	ocamlc -c $<

bindlib.cmx: bindlib.ml bindlib.cmi util.cmi
	ocamlopt -c $<

bindlib.cma: ptmap.cmo util.cmo bindlib.cmo
	ocamlc -o $@ -a $^

bindlib.cmxa: ptmap.cmx util.cmx bindlib.cmx
	ocamlopt -o $@ -a $^

## NVBindlib
nvbindlib.cmi: nvbindlib.mli util.cmi
	ocamlc -c $<

nvbindlib.cmo: nvbindlib.ml nvbindlib.cmi util.cmi
	ocamlc -c $<

nvbindlib.cmx: nvbindlib.ml nvbindlib.cmi util.cmi
	ocamlopt -c $<

nvbindlib.cma: ptmap.cmo util.cmo nvbindlib.cmo
	ocamlc -o $@ -a $^

nvbindlib.cmxa: ptmap.cmx util.cmx nvbindlib.cmx
	ocamlopt -o $@ -a $^

## Clean
clean:
	rm -rf *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
