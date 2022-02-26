Bindlib 6.0.0 (upcoming)
========================

Breaking changes:
- Major change of semantics that impacts variable renaming (and printing). The
  new semantics is explained in the documentation (see the `.mli` file), and a
  guide for porting code is provided below.

Other changes:
- Compilation using Dune.
- Generation of the `bindlib.opam` file using Dune.
- Require at least OCaml 4.07.0 (not strictly necessary, but solves CI issues
  with the installation of Dune on older versions).
- Cleaning up of the documentation in the code (`.ml` file).
- Polishing of the interface documentation (`.mli` file).
- Add the `unbind2_in` function (similar to `unbind2` but with a context).
- Add the `unmbind2_in` function (similar to `unmbind2` but with a context).
- Add the `free_vars` function to construct a context containing all the free
  variables in a boxed value.
- Add the `reserve_name` function to reserve a variable name in a context.
- Add a functor `Ctxt` that can be used to define a custom variable renaming
  policy, given a type of contexts and a few configurations.

This releases breaks your printing code: here is how to fix it
--------------------------------------------------------------

**Note: only the printing code is broken, all the rest is fine.**

To understand the problem, recall that variable substitutions does not perform
any bound variable renaming in Bindlib. What used to be the case, was that the
bound variable names were appropriately updated upon term construction. A term
that was freshly constructed was hence guaranteed to be safe for printing, and
thus it was advised to do a "cleanup" term traversal before printing (this was
done by composing a user-defined boxing function of type `t -> t Bindlib.box`,
and the build-in `Bindlib.unbox` function).

For example, with a type of the terms of the λ-calculus, that would be done as
follows, using the appropriate smart constructors.
```ocaml
type term =
  | Var of term Bindlib.var
  | Abs of (term, term) Bindlib.binder
  | App of term * term

let var = Bindlib.box_var
let abs = Bindlib.box_apply (fun b -> Abs(b))
let app = Bindlib.box_apply2 (fun t u -> App(t,u))

let rec box_term: term -> term Bindlib.box = fun t ->
  match t with
  | Var(x)   -> var x
  | Abs(b)   -> abs (Bindlib.box_binder box_term b)
  | App(t,u) -> app (box_term t) (box_term u)

(* DOES NOT GUARANTEE THAT THE OUTPUT TERM IS SAFE FOR PRINTING ANYMORE. *)
let cleanup: term -> term = fun t ->
  Bindlib.unbox (box_term t)

(* Naive printing function assuming correct names. *)
let rec to_string: term -> string = fun t ->
  match t with
  | Var(x)   -> Bindlib.name_of x
  | Abs(b)   -> let (x,t) = Bindlib.unbind b in
                "λ" ^ Bindlib.name_of x ^ "." ^ to_string t
  | App(t,u) -> "(" ^ to_string t ^ ") " ^ to_string u

(* BROKEN PRINTING FUNCTION AS OF BINDLIB VERSION 6.0.0. *)
let to_string t = to_string (cleanup t)
```

### What do we do now?

In this new version of Bindlib, a function that prints or displays a term must
rely on a context (of type `Bindlib.ctxt`) for free variables. All renaming is
performed when introducing fresh variables to substitute binders using special
functions like `Bindlib.new_var_in` or `Bindlib.unbind_in`, which take care of
picking a fresh name and maintaining the context.

Going back to the λ-calculus, the printing function must become the following.
```ocaml
(** [to_string_in ctxt t] is safe for printing assuming that [ctxt] is a valid
    context for [t] (i.e., it contains all of its free variables). *)
let rec to_string_in : ctxt -> term -> string = fun ctxt t ->
  match t with
  | Var(x)   -> Bindlib.name_of x
  | Abs(b)   -> let (x,t,ctxt) = Bindlib.unbind_in ctxt b in
                "λ" ^ Bindlib.name_of x ^ "." ^ to_string_in ctxt t
  | App(t,u) -> "(" ^ to_string_in ctxt t ^ ") " ^ to_string_in ctxt u

(** [to_string t] is always safe for printing. *)
let to_string : term -> string = fun t ->
  to_string_in (Bindlib.free_vars (box_term t)) t
```

In some situations where a form of context is required anyway (e.g., for doing
type-checking), it might make sense to maintain a Bindlib context to avoid the
cost of a call to `box_term` at printing time. However, this also means that a
small cost for maintaining names is payed early on.


Bindlib 5.0.1 (21/08/2018)
==========================

Bug fixes:
- Partially-applied `new_var` function could lead to variables with the same
  unique identifier (fixed by an η-expansion).
- The case where `unmbind2` was called with multiple binders with different
  arities was not handled, which could lead to problems (fixed by  raising
  `Invalid_argument "Arity missmatch in unmbind2"` on an arity mismatch).
- The `eq_mbinder` function also suffered from the same problem as `unmbind2`
  (fixed by returning `false` on an arity mismatch).

Other changes:
- Documentation update for `unmbind2` and `eq_mbinder`.


Bindlib 5.0.0 (27/05/2018)
==========================

Breaking changes:
- Rename type `'a bindbox` into `'a box`.
- Variance annotations removed on the `('a,'b) binder` type.
- Remove the `bind` function (use `bind_var` instead).
- Remove the `mbind` function (use `bind_mvar` instead).
- Remove the (unsafe) `binder_compose_left` function.
- Rename the (less unsafe) `binder_compose_right` into `binder_compose`.
- Remove the (unsafe) `binder_from_fun` function (replaced by `raw_binder`).
- Remove the (unsafe) `mbinder_from_fun` function (replaced by `raw_mbinder`).
- Remove the `bind_in` function.
- Rename the `box_of_var` function into `box_var`.
- Change the order of arguments of `copy_var` to match `new_var`.
- Remove the first argument of `eq_binder` (not necessary anymore).
- Remove the first argument of `eq_mbinder` (not necessary anymore).
- Remove the `fixpoint` function (too specific, and expressible).
- Remove the `is_substituted` function (semantics not clear).
- Remove the `prefix_of` function (abstraction-breaking).
- Remove the `suffix_of` function (abstraction-breaking).
- Remove the first argument of `unbind` (not necessary anymore).
- Remove the first argument of `unbind2` (not necessary anymore).
- Remove the first argument of `unbind_in` (not necessary anymore).
- Remove the first argument of `unmbind` (not necessary anymore).
- Remove the first argument of `unmbind2` function (not necessary anymore).
- Remove the first argument of `unmbind_in` (not necessary anymore).
- Remove the `vbind` function (use `bind_var` instead).

Other changes:
- Major improvement of the documentation, and description of the library in a
  [paper](https://dblp.uni-trier.de/rec/journals/corr/abs-1807-01872.html).
- Add the `box_binder` function (useful helper).
- Add the `box_mbinder` function (useful helper).
- Add the (unsafe) `mbinder_compose` function.
- Add the (unsafe) `raw_binder` function to construct binders "manually".
- Add the (unsafe) `raw_mbinder` function (similar to `raw_binder`).
- Add the `uids_of` function (similar to `uid_of`).


Bindlib 4.0.5 (24/01/2018)
==========================

Breaking changes:
- Remove the `free_of` function (users must now remember the injection).

Other changes:
- Add the (unsafe) `uid_of` function that can be used to obtain the unique
  identifier associated to a variable (useful for hashing).
- Add the `unbind2` function that destructs two binders at once with the same
  (fresh) variable.
- Add the `eq_binder` function for testing equality of binders given an
  equality function for the bodies.
- Add the `unmbind2` function (similar to `unbind2`).
- Add the `eq_binder` function (similar to `eq_binder`).
- Add the `is_substituted` function.


Bindlib 4.0.4 (21/08/2017)
==========================

Breaking changes:
- Rename `f` into `lift_box` in the signature of the `Lift` functor.
- Rename `f` into `lift_box` in the signature of the `Lift2` functor.
- Remove the (unsafe) `apply_in_box` function.
- Remove the (useless) `list_vars` function.

Other changes:
- Require at least OCaml 3.12.1.
- Major cleaning up and documentation effort.
- Move from `darcs` to `git`.
- Add the `mbinder_from_fun` function (similar to `binder_from_fun`).
- Add the `mbinder_occurs` function (similar to `binder_occur`).
- Add the `mbinder_rank` function (similar to `binder_rank`).
- Add the `suffix_of` function to query the suffix of a variable name.


Bindlib 4.0.3
=============

Breaking changes:
- Rename the `'a variable` type into `'a var`.
- Rename the `'a mvariable` type into `'a mvar`.
- Rename the `compare_variables` function into `compare_vars`.
- Rename the `eq_variables` function into `eq_vars`.
- Rename the `context` type into `ctxt`.
- Rename the `list_variables` function into `list_vars`.

Other changes:
- Add the `prefix_of` function to query the prefix of a variable name.
- Add the `unmbind` function which destructs a multiple binder (like the
  `unbind` function for binders).
- Add the `unbind_in` and `unmbind_in` functions, which are similar to
  `unbind` and `unmbind` but work with a context.
- Remove the `Bindlib_util` and `Ptmap` modules (not meant to be exposed).


Bindlib 4.0.2
=============

Breaking changes:
- Remove the second (`int`) argument of the unsafe `binder_from_fun` function,
  which also has now a different semantics.

Other changes:
- Document the `binder_from_fun` function (unsafe).
- Add the `box_apply4` function.


Bindlib 4.0
===========

Major cleanup/rewriting of the library by Rodolphe Lepigre.
First version distributed via Opam.


Earlier versions
================

All earlier versions of Bindlib were developed by Christophe Raffalli.
A version for Haskell was even available at some point.
