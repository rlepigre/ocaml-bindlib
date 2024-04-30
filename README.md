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
 - [The Catala language](https://catala-lang.org/)
 - Many more small projects...

Dependencies
------------

List of dependencies:
 - OCaml (at least version 4.07.0),
 - The Dune build system (at least version 2.7.0).

Installation
------------

You can either pin the repository with opam or run the following.
```bash
make
make install
```

Contributors
------------

Main contributors:
- Christophe Raffalli ([@craff](https://github.com/craff), author of the first version)
- Rodolphe Lepigre ([@rlepigre](https://github.com/rlepigre))

The project received additional contributions from:
- Alain ([@adelaett](https://github.com/adelaett))
- Rémi Nollet ([@rnollet](https://github.com/rnollet))
- Qiancheng Fu ([@qcfu-bu](https://github.com/qcfu-bu))
