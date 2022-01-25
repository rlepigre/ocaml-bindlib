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
   "Bindlib is a library allowing the manipulation of data structures \
    with bound variables. It is particularly useful when writing ASTs \
    for programming languages, but also for manipulating terms of the \
    λ-calculus or quantified formulas.")
 (depends
   (ocaml (>= 4.02.0))
   (dune :build)
   (timed (and (= 1.0) :with-test))
   (earley (and (= 3.0.0) :with-test))
   (odoc :with-doc)))
