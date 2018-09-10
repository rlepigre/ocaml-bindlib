The Bindlib library for OCaml
=============================

Bindlib is a library allowing the manipulation of data structures
with bound variables. It is particularly useful when writing ASTs
for programming languages, but also for manipulating terms of the
λ-calculus or quantified formulas. In the internals,  binders are
represented using a form of higher-order abstract syntax (HOAS).

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
