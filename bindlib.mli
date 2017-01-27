(** The Bindlib Library provides datatypes to represent binders in
    arbitrary languages. The implementation is efficient and manages
    name in the expected way (variables have a prefered name to which
    an integer suffix can be added to avoid capture during
    substitution.

    Author: Christophe Raffalli and Rodolphe Lepigre
*)

(** To build an abstract syntax tree using bindlib, one will need to make use
of the following types for variables and binders. *)

(** Type of a variable of type ['a]. *)
type 'a var

(** Type of a multi-variable of type ['a]. *)
type 'a mvar = 'a var array

(** Type of a binder for a variable of type ['a] in a term of type ['b']. *)
type (-'a,+'b) binder

(** Type of a multi-binder for a multi-variable of type ['a] in a term of type
['b]. *)
type ('a,'b) mbinder

(** Substitution functions. *)
val subst  : ('a,'b) binder -> 'a -> 'b
val msubst : ('a,'b) mbinder -> 'a array -> 'b

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

val mbinder_arity    : ('a,'b) mbinder -> int
val mbinder_names    : ('a,'b) mbinder -> string array
val mbinder_constant : ('a,'b) mbinder -> bool
val mbinder_closed   : ('a,'b) mbinder -> bool

(** Utility functions on variables. *)
val name_of  : 'a var -> string
val prefix_of  : 'a var -> string
val free_of  : 'a var -> 'a
val hash_var : 'a var -> int

(** Safe comparision of variables. *)
val compare_vars : 'a var -> 'b var -> int
val eq_vars : 'a var -> 'b var -> bool

(** Creates a copy of the given variable that is not distinguishable from the
original when bound. However, when it is free (that is not bound when calling
unbox), it might be made free in a different way. For instance, its name or
syntactic wrapper may be different. *)
val copy_var : 'b var -> string -> ('a var -> 'a) -> 'a var


(** To work with term containing free variables (that might be bound at some
point), Bindlib provides the following datatype. This type of "terms under
construction" will provide an efficient way of binding variables in a term
of type ['a]. *)
type (+'a) bindbox

(** Once the construction of an expression of type ['a] is finished, the
function [unbox] need to be called in order to obtain the built expression. *)
val unbox : 'a bindbox -> 'a

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

(** A function to apply a function under a bindbox, the function is
    applied immediately ... the list of free variables is not
    updated, so it may be too large *)
val apply_in_box : ('a -> 'b) -> 'a bindbox -> 'b bindbox


(** Useful function to work with "higher order variables", that is variables
representing a binder themselves (that can hence be applied to arguments. *)
val bind_apply  : ('a, 'b) binder bindbox  -> 'a bindbox       -> 'b bindbox
val mbind_apply : ('a, 'b) mbinder bindbox -> 'a array bindbox -> 'b bindbox

(** Very advanced feature: binder fixpoint. *)
val fixpoint : (('a, 'b) binder, ('a, 'b) binder) binder bindbox
  -> ('a, 'b) binder bindbox

(** Reset the counter that provides fresh keys for variables. To be used with
care. *)
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
  val f : 'a bindbox M.t -> 'a M.t bindbox
end

module Lift2(M: Map2) : sig
  val f : ('a bindbox, 'b bindbox) M.t -> ('a, 'b) M.t bindbox
end

(** Here are some functions defined using the functorial interface. They lift
the ['a bindbox] type over the ['a list] or ['a array] types for instance. *)
val box_list     : 'a bindbox list  -> 'a list  bindbox
val box_rev_list : 'a bindbox list  -> 'a list  bindbox
val box_array    : 'a bindbox array -> 'a array bindbox



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

(** For debugging. *)
val list_vars : 'a bindbox -> string list
