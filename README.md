The Bindlib library for OCaml
=============================

Bindlib is a library allowing the manipulation of data structures
with bound variables. It is particularly useful when writing ASTs
for programming languages, but also for manipulating terms of the
λ-calculus or quantified formulas. In the internals,  binders are
represented using a form of higher-order abstract syntax (HOAS).

Ressources:
 - [Introductory paper](https://eptcs.web.cse.unsw.edu.au/paper.cgi?LFMTP2018.4)
 - [Generated documentation](https://rlepigre.github.io/ocaml-bindlib/ocamldoc/Bindlib.html)

Projects using Bindlib:
 - [Lambdapi (implementation of Dedukti)](https://github.com/rlepigre/lambdapi)
 - [PML₂ language](https://github.com/rlepigre/pml)
 - [SubML language](https://rlepigre.github.io/subml/)
 - [PML](https://lama.univ-savoie.fr/tracpml)
 - Many more small projects...

Dependencies
------------

List of dependencies:
 - OCaml (at least 3.12)
 - Findlib (build)
 - OCamlbuild (build)
 - GNU Make (build)

Installation
------------

```bash
make
make install
```

Other things
------------

To generate the documentation (ocamldoc):
```bash
make doc
```

To build the examples:
```bash
make examples
make tests
```
