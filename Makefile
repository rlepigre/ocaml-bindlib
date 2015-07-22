all: bindlib.cma bindlib.cmxa nvbindlib.cma nvbindlib.cmxa

## Ptmap module
ptmap.cmi: ptmap.mli
	ocamlc -c $<

ptmap.cmo: ptmap.ml ptmap.cmi
	ocamlc -c $<

ptmap.cmx: ptmap.ml ptmap.cmi
	ocamlopt -c $<

## Bindlib
bindlib.cmi: bindlib.mli ptmap.cmi
	ocamlc -c $<

bindlib.cmo: bindlib.ml bindlib.cmi ptmap.cmi
	ocamlc -c $<

bindlib.cmx: bindlib.ml bindlib.cmi ptmap.cmi
	ocamlopt -c $<

bindlib.cma: ptmap.cmo bindlib.cmo
	ocamlc -o $@ -a $^

bindlib.cmxa: ptmap.cmx bindlib.cmx
	ocamlopt -o $@ -a $^

## NVBindlib
nvbindlib.cmi: nvbindlib.mli ptmap.cmi
	ocamlc -c $<

nvbindlib.cmo: nvbindlib.ml nvbindlib.cmi ptmap.cmi
	ocamlc -c $<

nvbindlib.cmx: nvbindlib.ml nvbindlib.cmi ptmap.cmi
	ocamlopt -c $<

nvbindlib.cma: ptmap.cmo nvbindlib.cmo
	ocamlc -o $@ -a $^

nvbindlib.cmxa: ptmap.cmx nvbindlib.cmx
	ocamlopt -o $@ -a $^

## Clean
clean:
	rm -rf *.cmi *.cmx *.cmo *.cma *.cmxa *.o *.a

distclean: clean
