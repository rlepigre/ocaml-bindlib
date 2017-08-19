(****************************************************************************
 * The Bindlib Library provides an efficient representation for binders (or *
 * structures with bound variables). Names are managed in the expected way, *
 * by performing minimal renaming (an integer suffix is increased in case a *
 * change of name is required).                                             *
 *                                                                          *
 * Authors:                                                                 *
 *   - Christophe Raffalli <christophe.raffalli@univ-smb.fr>                *
 *   - Rodolphe Lepigre <rodolphe.lepigre@univ-smb.fr>                      *
 ****************************************************************************)

(** The [Bindlib] library provides support for free and bound variables in the
    OCaml language. The main application is the construction of abstract types
    containing a binding structure (e.g., abstract syntax trees).

    @author Christophe Raffalli
    @author Rodolphe Lepigre *)


(** {2 Variables, binders and substitution} *)


(** The [Bindlib] library provides two type constructors for building abstract
    syntax trees: ['a var] and [('a,'b) binder]. Intuitively, ['a var] will be
    a representation for a free variable of type ['a],  and [('a,'b) binder] a
    represention for a term of type ['b] depdening on a variable (or value) of
    type ['a] (the type [('a,'b) binder] can be seen as ['a -> 'b]). Note that
    types ['a mvar] and [('a,'b) mbinder] are provided for handling arrays  of
    variables. *)

(** Type of a free variable of type ['a]. *)
type 'a var

(** Type of an array of variables of type ['a]. *)
type 'a mvar = 'a var array

(** Type of a binder for an element of type ['a] into an element of type ['b].
    In terms of higher-order abstract syntax, it can be seen as ['a -> 'b]. *)
type (-'a,+'b) binder

(** Type of a binder for an array of elements of type ['a] into an element  of
    type ['b]. *)
type ('a,'b) mbinder

(** As an example, we give bellow the definition of a simple representation of
    the terms of the lambda-calculus. {[
    type term =
      | Var of term var
      | Abs of (term, term) binder
      | App of term * term ]} *)

(** [subst b v] substitutes (using application) the variable bound by [b] with
    the value [b]. This is a very efficient operations. *)
val subst  : ('a,'b) binder -> 'a -> 'b

(** [msubst b vs] substitutes (using application) the array of variables bound
    by [b] with the values [vs]. This is a very efficient operations. However,
    the length of the [vs] array should match the arity of the binder (see the
    function [mbinder_arity]). *)
val msubst : ('a,'b) mbinder -> 'a array -> 'b

(** Comming back to our lambda-calculus example,  we can define the evaluation
    of a lambda-term as a simple recursive function using [subst]. {[
    let rec eval : term -> term = fun t ->
      match t with
      | App(f,a) ->
          begin
            match eval f with
            | Abs(b) -> eval (subst b a)
            | _      -> t
          end
      | _        -> t ]} *)


(** {2 Constructing terms in the bindbox} *)


(** One of the main design priciple of the [Bindlib] library is efficiency. To
    obtain very fast substitutions, a price is paid at the construction of the
    terms. Indeed,  binders (i.e., element of type [('a,'b) binder]) cannot be
    defined directly. Instead, they are put together in the type ['a bindbox].
    It correspond to a term of type ['a] with free variables that may still be
    bound in the future. *)

(** Type of a term of type ['a] under construction. Using this representation,
    free variables can be bound easily. *)
type +'a bindbox

(** [unbox e] can be called when the construction of a term is finished (e.g.,
    when the desired variables have all been bound). *)
val unbox : 'a bindbox -> 'a


(** {2 ... (work in progress)} *)


(** Variable creation functions. *)
val new_var  : ('a var -> 'a) -> string       -> 'a var
val new_mvar : ('a var -> 'a) -> string array -> 'a mvar



(** Utility functions on binders. *)
val binder_name     : ('a,'b) binder -> string
val binder_occur    : ('a,'b) binder -> bool
val binder_rank     : ('a,'b) binder -> int
val binder_constant : ('a,'b) binder -> bool
val binder_closed   : ('a,'b) binder -> bool
val binder_compose_left  : ('a -> 'b) -> ('b,'c) binder -> ('a,'c) binder
val binder_compose_right : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder

(** binder from fun, transform a function into a binder,
    but the function will only be called when the
    binder is substituted. This is not the normal way to build
    binder. Still it may be a good way, for instance to contract
    two binders into one without copying the whole structure.
    Ex: to transform (lam x lam y.t(x,y)) into (lam x.t(x,x))
*)
val binder_from_fun : string -> ('a -> 'b) -> ('a,'b) binder
val mbinder_from_fun : string array -> ('a array -> 'b) -> ('a,'b) mbinder

val mbinder_arity    : ('a,'b) mbinder -> int
val mbinder_names    : ('a,'b) mbinder -> string array
val mbinder_constant : ('a,'b) mbinder -> bool
val mbinder_closed   : ('a,'b) mbinder -> bool
val mbinder_rank     : ('a,'b) mbinder -> int

(** Utility functions on variables. *)
val name_of   : 'a var -> string
val prefix_of : 'a var -> string
val suffix_of : 'a var -> int
val free_of   : 'a var -> 'a
val hash_var  : 'a var -> int

(** Safe comparision of variables. *)
val compare_vars : 'a var -> 'b var -> int
val eq_vars : 'a var -> 'b var -> bool

(** Creates a copy of the given variable that is not distinguishable from the
original when bound. However, when it is free (that is not bound when calling
unbox), it might be made free in a different way. For instance, its name or
syntactic wrapper may be different. *)
val copy_var : 'b var -> string -> ('a var -> 'a) -> 'a var



(** Build a ['a bindbox] from a ['a var]. *)
val box_of_var : 'a var -> 'a bindbox

(** Put a term into a bindbox. None of the variables of the given term (if any)
will be considered free. Hence no variables of the term will be available for
binding. *)
val box : 'a -> 'a bindbox

(** Application operator in the [_ bindbox] data structure. This allows the
construction of expressions by applying a function with free variables to an
argument with free variables. *)
val apply_box : ('a -> 'b) bindbox -> 'a bindbox -> 'b bindbox

(** Is a ['a bindbox] closed? The function returns [true] if the ['a bindbox]
has no free variables, and [false] otherwise. *)
val is_closed : 'a bindbox -> bool

(** Test if a given ['a var] occur in a given ['b bindbox]. *)
val occur : 'a var -> 'b bindbox -> bool

(** Dummy bindbox to be used in uninitialised structures (e.g. array creation).
If [unbox] is called on a data structure containing a [dummy_bindbox] then the
exception [Failure "Invalid use of dummy_bindbox"] is raised. *)
val dummy_bindbox : 'a bindbox

(** Building of binders. *)
val bind  : ('a var -> 'a) -> string -> ('a bindbox -> 'b bindbox)
  -> ('a,'b) binder bindbox
val mbind : ('a var -> 'a) -> string array
  -> ('a bindbox array -> 'b bindbox) -> ('a,'b) mbinder bindbox
(** Building of binders. *)
val vbind  : ('a var -> 'a) -> string -> ('a var -> 'b bindbox)
  -> ('a,'b) binder bindbox
val mvbind : ('a var -> 'a) -> string array
  -> ('a var array -> 'b bindbox) -> ('a,'b) mbinder bindbox

(** Breaking binders. *)
val unbind : ('a var -> 'a) -> ('a,'b) binder -> 'a var * 'b
val unmbind : ('a var -> 'a) -> ('a,'b) mbinder -> 'a mvar * 'b

(** Variable binding. *)
val bind_var  : 'a var  -> 'b bindbox -> ('a, 'b) binder bindbox
val bind_mvar : 'a mvar -> 'b bindbox -> ('a, 'b) mbinder bindbox


(** The following functions can be written using [box] and [apply_box]. Here,
they are implemented differently for optimisation purposes. We give the
equivalent function using [box] and [apply_box] in comments. *)

(** [box_apply f a = apply_box (box f) a] *)
val box_apply : ('a -> 'b) -> 'a bindbox -> 'b bindbox

(** [box_apply2 f a b = apply_box (apply_box (box f) a) b] *)
val box_apply2 : ('a -> 'b -> 'c) -> 'a bindbox -> 'b bindbox -> 'c bindbox

(** [box_apply3 f a b c = apply_box (apply_box (apply_box (box f) a) b) c] *)
val box_apply3 : ('a -> 'b -> 'c -> 'd) -> 'a bindbox -> 'b bindbox
  -> 'c bindbox -> 'd bindbox

(** same as above with more arguments *)
val box_apply4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a bindbox -> 'b bindbox
  -> 'c bindbox -> 'd bindbox -> 'e bindbox

(** [box_pair (x,y) = box_apply2 (fun a b -> (a,b)) x y] *)
val box_pair : 'a bindbox -> 'b bindbox -> ('a * 'b) bindbox
val box_triple : 'a bindbox -> 'b bindbox -> 'c bindbox
  -> ('a * 'b * 'c) bindbox

val box_opt : 'a bindbox option -> 'a option bindbox

(** Advanced features on the ['a bindbox] type. *)

(** Useful function to work with "higher order variables", that is variables
representing a binder themselves (that can hence be applied to arguments. *)
val bind_apply  : ('a, 'b) binder bindbox  -> 'a bindbox       -> 'b bindbox
val mbind_apply : ('a, 'b) mbinder bindbox -> 'a array bindbox -> 'b bindbox

(** Very advanced feature: binder fixpoint. *)
val fixpoint : (('a, 'b) binder, ('a, 'b) binder) binder bindbox
  -> ('a, 'b) binder bindbox

(** Reset the counter that provides fresh keys for variables. *)
val reset_counter : unit -> unit

(** To work with the [bindbox] type more conveniently in conjunction with data
with a map structire, the following functors are provided. *)

module type Map = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Map2 = sig
  type ('a, 'b) t
  val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
end

module Lift(M: Map) : sig
  val lift_box : 'a bindbox M.t -> 'a M.t bindbox
end

module Lift2(M: Map2) : sig
  val lift_box : ('a bindbox, 'b bindbox) M.t -> ('a, 'b) M.t bindbox
end

(** Here are some functions defined using the functorial interface. They lift
the ['a bindbox] type over the ['a list] or ['a array] types for instance. *)
val box_list     : 'a bindbox list  -> 'a list  bindbox
val box_rev_list : 'a bindbox list  -> 'a list  bindbox
val box_array    : 'a bindbox array -> 'a array bindbox



(** {2 Working in a context} *)


(** It is sometimes convenient to work in a context for variables. This is
useful, in particular, to reserve variable names. To do so, Bindlib provides
a type for contexts together with functions for creating variables and binding
variables in a context. *)

(** Type of a context. *)
type ctxt

(** Empty context. *)
val empty_ctxt : ctxt

(** Fresh variable creation in a context (corresponds to [new_var]). *)
val new_var_in : ctxt -> ('a var -> 'a) -> string
  -> 'a var * ctxt

(** Similar function for multi-variables. *)
val new_mvar_in : ctxt -> ('a var -> 'a) -> string array
  -> 'a mvar * ctxt

(** Breaking binders. *)
val unbind_in  : ctxt -> ('a var -> 'a) -> ('a,'b)  binder -> 'a  var * 'b * ctxt
val unmbind_in : ctxt -> ('a var -> 'a) -> ('a,'b) mbinder -> 'a mvar * 'b * ctxt

(** Binding operation in a context (corresponds to [bind]). *)
val bind_in : ctxt -> ('a var -> 'a) -> string
  -> ('a bindbox -> ctxt -> 'b bindbox) -> ('a,'b) binder bindbox

(** Similar function for multi-binders. *)
val mbind_in : ctxt -> ('a var -> 'a) -> string array
  -> ('a bindbox array -> ctxt -> 'b bindbox) -> ('a,'b) mbinder bindbox
