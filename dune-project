(lang dune 2.7)

(name "bindlib")
(authors
  "Rodolphe Lepigre <lepigre@mpi-sws.org>"
  "Christophe Raffalli <christophe@raffalli.eu")
(maintainers
  "Rodolphe Lepigre <lepigre@mpi-sws.org>"
  "Christophe Raffalli <christophe@raffalli.eu")
(source (github rlepigre/bindlib))
(license "LGPL-3.0")

(generate_opam_files true)

(package
 (name bindlib)
 (synopsis "OCaml Bindlib library for bound variables")
 (description
   "The Bindlib library provides support for free and bound variables in the
    OCaml language. The main application is the representation of types with
    a binding structure (e.g., abstract syntax trees).")
 (depends
   (ocaml (>= 4.07.0))
   (dune :build)
   (timed (and (= 1.0) :with-test))
   (earley (and (= 3.0.0) :with-test))
   (odoc :with-doc)))
