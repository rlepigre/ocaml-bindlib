(lang dune 2.7)
(name "bindlib")
(generate_opam_files true)

(authors
 "Rodolphe Lepigre <lepigre@mpi-sws.org>"
 "Christophe Raffalli <christophe@raffalli.eu")
(maintainers
 "Rodolphe Lepigre <lepigre@mpi-sws.org>"
 "Christophe Raffalli <christophe@raffalli.eu")

(source (github rlepigre/ocaml-bindlib))
(license "LGPL-3.0-only")

(package
 (name bindlib)
 (synopsis "OCaml Bindlib library for bound variables")
 (description
   "The Bindlib library provides support for free and bound variables in the
    OCaml language. The main application is the representation of types with
    a binding structure (e.g., abstract syntax trees).")
 (depends
  (ocaml (>= 4.14.0))
  (dune :build)
  (timed (and (>= 1.0) :with-test))
  (pacomb (and (>= 1.1) :with-test))
  (hashcons (and (>= 1.4.0) :with-test))))
