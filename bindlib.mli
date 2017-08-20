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

(** [new_var mkfree name] creates a new variable using a function [mkfree] and
    a [name]. The [mkfree] function is used to inject variables in the type of
    the corresponding elements. *)
val new_var  : ('a var -> 'a) -> string       -> 'a var

(** [new_mvar mkfree names] creates a new array of variables using a  function
    [mkfree] (see [new_var]) and  a [name]. *)
val new_mvar : ('a var -> 'a) -> string array -> 'a mvar

(** Following on our example of the lambda-calculus, the [mkfree] function for
    variables of type [term var] could be defined as follows. {[
    let mkfree : term var -> term = fun x -> Var(x) ]} *)

(** [name_of x] returns a printable name for variable [x]. *)
val name_of   : 'a var -> string

(** [unbind mkfree b] breaks down the binder [b] into a variable, and the term
    in which this variable is now free.  Note that the usual [mkfree] function
    is required,  since [unbind] needs to create a new variable (its name will
    be that of the previously bound variable). *)
val unbind : ('a var -> 'a) -> ('a,'b) binder -> 'a var * 'b

(** [unmbind mkfree b] breaks down the binder [b] into variables, and the term
    in which these variables are now free. Again,  the usual [mkfree] function
    is required, and the name of the new variables is based on that of all the
    variables that were previously bound. *)
val unmbind : ('a var -> 'a) -> ('a,'b) mbinder -> 'a mvar * 'b

(** An usual use of [unbind] is the wrinting of pretty-printing functions. The
    function given bellow transforms a lambda-term into a [string].  Note that
    the [name_of] function is used for variables. {[
    let rec to_string : term -> string = fun t ->
      match t with
      | Var(x)   -> name_of x
      | Abs(b)   -> let (x,t) = unbind mkfree b in
                    "\\" ^ name_of x ^ "." ^ to_string t
      | App(t,u) -> "(" ^ to_string t ^ ") " ^ to_string u ]} *)


(** {2 Constructing terms and binders in the bindbox} *)


(** One of the main design priciple of the [Bindlib] library is efficiency. To
    obtain very fast substitutions, a price is paid at the construction of the
    terms. Indeed,  binders (i.e., element of type [('a,'b) binder]) cannot be
    defined directly. Instead, they are put together in the type ['a bindbox].
    It correspond to a term of type ['a] with free variables that may still be
    bound in the future. *)

(** Type of a term of type ['a] under construction. Using this representation,
    free variables can be bound easily. *)
type +'a bindbox

(** [box_of_var x] builds a ['a bindbox] from the ['a var] [x]. *)
val box_of_var : 'a var -> 'a bindbox

(** [box e] puts the value [e] into the ['a bindbox] type, assuming that it is
    closed. Thus, if [e] contains variables,  then they will not be considered
    free. This means that no variable of [e] will be available for binding. *)
val box : 'a -> 'a bindbox

(** [apply_box bf ba] applies the boxed function [bf] to a boxed argument [ba]
    inside the ['a bindbox] type. This function is useful for constructing new
    expressions by applying a function with free variables to an argument with
    free variables. Note that the ['a bindbox] type is an applicative functor.
    Its application operator (sometimes written "<*>") is [apply_box], and its
    unit (sometimes called "pure") is [box]. *)
val apply_box : ('a -> 'b) bindbox -> 'a bindbox -> 'b bindbox

(** [box_apply f ba] applies the function [f] to a boxed argument [ba].  It is
    equivalent to [apply_box (box f) ba], but is more efficient. *)
val box_apply : ('a -> 'b) -> 'a bindbox -> 'b bindbox

(** [box_apply2 f ba bb] applies the function [f] to two boxed arguments  [ba]
    and [bb]. It is equivalent to [apply_box (apply_box (box f) ba) bb] but it
    is more efficient. *)
val box_apply2 : ('a -> 'b -> 'c) -> 'a bindbox -> 'b bindbox -> 'c bindbox

(** [bind mkfree name f] constructs a boxed binder from the function [f]. Note
    that [name] and [mkfree] are required to build the bound variable. *)
val bind  : ('a var -> 'a) -> string -> ('a bindbox -> 'b bindbox)
  -> ('a,'b) binder bindbox

(** [mbind mkfree names f] constructs a boxed binder from function [f],  using
    [mkfree] and [names] to build the bound variables. *)
val mbind : ('a var -> 'a) -> string array
  -> ('a bindbox array -> 'b bindbox) -> ('a,'b) mbinder bindbox

(** As mentioned earlier,  terms with bound variables can only be build in the
    ['a bindbox] type. To ease the writing of terms,  it is a good practice to 
    define "smart constructors" at the ['a bindbox] level.  Coming back to our
    lambda-calculus example, we can give the following smart constructors. {[
    let var : string -> term bindbox =
      fun x -> box_of_var (new_var mkfree x)

    let lam : string -> (term bindbox -> term bindbox) -> term bindbox =
      fun x f -> box_apply (fun b -> Lam(b)) (bind mkfree x f)

    let app : term bindbox -> term bindbox -> term bindbox =
      fun t u -> box_apply2 (fun t u -> App(t,u)) t u ]} *)

(** [unbox e] can be called when the construction of a term is finished (e.g.,
    when the desired variables have all been bound). *)
val unbox : 'a bindbox -> 'a

(** We can then easily define terms of the lambda-calculus as follows. {[
    let id    : term = (* \x.x *)
      unbox (lam "x" (fun x -> x))

    let fst   : term = (* \x.\y.x *)
      unbox (lam "x" (fun x -> lam "y" (fun _ -> x)))

    let omega : term = (* (\x.(x) x) \x.(x) x *)
      let delta = lam "x" (fun x -> app x x) in
      unbox (app delta delta) ]} *)

(* FIXME FIXME FIXME chantier en cours FIXME FIXME FIXME *)

val vbind  : ('a var -> 'a) -> string -> ('a var -> 'b bindbox)
  -> ('a,'b) binder bindbox

val mvbind : ('a var -> 'a) -> string array
  -> ('a var array -> 'b bindbox) -> ('a,'b) mbinder bindbox

val bind_var  : 'a var  -> 'b bindbox -> ('a, 'b) binder bindbox

val bind_mvar : 'a mvar -> 'b bindbox -> ('a, 'b) mbinder bindbox


(** {2 Attributes of variables, binders and bindboxes} *)


(** Utility functions on binders. *)
val binder_name     : ('a,'b) binder -> string
val binder_occur    : ('a,'b) binder -> bool
val binder_rank     : ('a,'b) binder -> int
val binder_constant : ('a,'b) binder -> bool
val binder_closed   : ('a,'b) binder -> bool
val binder_compose_left  : ('a -> 'b) -> ('b,'c) binder -> ('a,'c) binder
val binder_compose_right : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder

val mbinder_arity    : ('a,'b) mbinder -> int
val mbinder_names    : ('a,'b) mbinder -> string array
val mbinder_constant : ('a,'b) mbinder -> bool
val mbinder_closed   : ('a,'b) mbinder -> bool
val mbinder_rank     : ('a,'b) mbinder -> int

(** Utility functions on variables. *)
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

(** Is a ['a bindbox] closed? The function returns [true] if the ['a bindbox]
has no free variables, and [false] otherwise. *)
val is_closed : 'a bindbox -> bool

(** Test if a given ['a var] occur in a given ['b bindbox]. *)
val occur : 'a var -> 'b bindbox -> bool


(** {2 More bindbox manipulation functions} *)


(** The following functions can be written using [box] and [apply_box]. Here,
they are implemented differently for optimisation purposes. We give the
equivalent function using [box] and [apply_box] in comments. *)

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


(** {2 Advanced features} *)

(** Useful function to work with "higher order variables", that is variables
representing a binder themselves (that can hence be applied to arguments. *)
val bind_apply  : ('a, 'b) binder bindbox  -> 'a bindbox       -> 'b bindbox
val mbind_apply : ('a, 'b) mbinder bindbox -> 'a array bindbox -> 'b bindbox

(** Very advanced feature: binder fixpoint. *)
val fixpoint : (('a, 'b) binder, ('a, 'b) binder) binder bindbox
  -> ('a, 'b) binder bindbox

(** Reset the counter that provides fresh keys for variables. *)
val reset_counter : unit -> unit

(** binder from fun, transform a function into a binder,
    but the function will only be called when the
    binder is substituted. This is not the normal way to build
    binder. Still it may be a good way, for instance to contract
    two binders into one without copying the whole structure.
    Ex: to transform (lam x lam y.t(x,y)) into (lam x.t(x,x))
*)
val binder_from_fun : string -> ('a -> 'b) -> ('a,'b) binder
val mbinder_from_fun : string array -> ('a array -> 'b) -> ('a,'b) mbinder

(** Dummy bindbox to be used in uninitialised structures (e.g. array creation).
If [unbox] is called on a data structure containing a [dummy_bindbox] then the
exception [Failure "Invalid use of dummy_bindbox"] is raised. *)
val dummy_bindbox : 'a bindbox


