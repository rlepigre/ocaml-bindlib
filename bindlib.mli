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
    the corresponding elements. It is a form of syntactic wrapper. *)
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
    the free variable of the term can be bound easily. *)
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
    let var : term var -> term bindbox =
      fun x -> box_of_var x

    let abs : string -> (term bindbox -> term bindbox) -> term bindbox =
      fun x f -> box_apply (fun b -> Abs(b)) (bind mkfree x f)

    let app : term bindbox -> term bindbox -> term bindbox =
      fun t u -> box_apply2 (fun t u -> App(t,u)) t u ]} *)

(** [unbox e] can be called when the construction of a term is finished (e.g.,
    when the desired variables have all been bound). *)
val unbox : 'a bindbox -> 'a

(** We can then easily define terms of the lambda-calculus as follows. {[
    let id    : term = (* \x.x *)
      unbox (abs "x" (fun x -> x))

    let fst   : term = (* \x.\y.x *)
      unbox (abs "x" (fun x -> abs "y" (fun _ -> x)))

    let omega : term = (* (\x.(x) x) \x.(x) x *)
      let delta = abs "x" (fun x -> app x x) in
      unbox (app delta delta) ]} *)

(** [vbind mkfree name f] constructs a boxed binder from the function [f],  as
    the [bind] function does. The difference here is that the domain of [f] is
    ['a var], and not ['a bindbox]. *)
val vbind  : ('a var -> 'a) -> string -> ('a var -> 'b bindbox)
  -> ('a,'b) binder bindbox

(** [mvbind mkfree names f] constructs a boxed binder from the function [f] as
    the [mbind] function does. However, the domain of [f] is ['a var], and not
    ['a bindbox]. *)
val mvbind : ('a var -> 'a) -> string array
  -> ('a var array -> 'b bindbox) -> ('a,'b) mbinder bindbox

(** Using the [vbind] function instead of the [bind] function,  we can give an
    alternative smart constructor for lambda-abstraction. Note that we need to
    use the [box_of_var] to use a variable when defining a term. {[
    let abs_var : string -> (term var -> term bindbox) -> term bindbox =
      fun x f -> box_apply (fun b -> Abs(b)) (vbind mkfree x f)

    let delta : term = (* \x.(x) x *)
      unbox (abs_var "x" (fun x -> app (var x) (var x))) ]} *)

(** [bind_var x b] binds the variable [x] in [b] to produce a boxed binder. In
    fact, is used to implement [bind] and [vbind]. *)
val bind_var  : 'a var  -> 'b bindbox -> ('a, 'b) binder bindbox

(** [bind_mvar xs b] binds the variables of [xs] in [b] to get a boxed binder.
    In fact, [bind_mvar] is used to implement [mbind] and [mvbind]. *)
val bind_mvar : 'a mvar -> 'b bindbox -> ('a, 'b) mbinder bindbox


(** {2 More bindbox manipulation functions} *)


(** In general, it is not difficult to use the [box] and [apply_box] functions
    to manipulate any kind of data in the ['a bindbox] type.  However, working
    with these functions alone can be tedious. The functions provided here can
    be used to manipulate standard data types in an optimised way. *)

(** [box_opt bo] shifts the [option] type of [bo] into the [bindbox]. *)
val box_opt : 'a bindbox option -> 'a option bindbox

(** [box_list bs] shifts the [list] type of [bs] into the [bindbox]. *)
val box_list : 'a bindbox list  -> 'a list  bindbox

(** [box_rev_list bs] shifts the [list] type of [bs] into the [bindbox], while
    reversing the list (for efficiency). *)
val box_rev_list : 'a bindbox list  -> 'a list  bindbox

(** [box_array bs] shifts the [array] type of [bs] into the [bindbox]. *)
val box_array : 'a bindbox array -> 'a array bindbox

(** [box_apply3] is similar to [box_apply2]. *)
val box_apply3 : ('a -> 'b -> 'c -> 'd) -> 'a bindbox -> 'b bindbox
  -> 'c bindbox -> 'd bindbox

(** [box_apply4] is similar to [box_apply2] and [box_apply3]. *)
val box_apply4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a bindbox -> 'b bindbox
  -> 'c bindbox -> 'd bindbox -> 'e bindbox

(** [box_pair ba bb] is the same as [box_apply2 (fun a b -> (a,b)) ba bb]. *)
val box_pair : 'a bindbox -> 'b bindbox -> ('a * 'b) bindbox

(** [box_trible] is similar to [box_pair], but for triples. *)
val box_triple : 'a bindbox -> 'b bindbox -> 'c bindbox
  -> ('a * 'b * 'c) bindbox

(** Type of a module equipped with a [map] function. *)
module type Map =
  sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

(** Functorial interface used to build lifting functions (i.e., functions that
    permute the ['a bindbox] type with another type constructor) for any  type
    equipped with a [map] function. *)
module Lift(M : Map) :
  sig
    val lift_box : 'a bindbox M.t -> 'a M.t bindbox
  end

(** Type of a module equipped with a "binary" [map] function. *)
module type Map2 =
  sig
    type ('a, 'b) t
    val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
  end

(** Similar to the [Lift] functor, but handles "binary" [map] functions. *)
module Lift2(M : Map2) :
  sig
    val lift_box : ('a bindbox, 'b bindbox) M.t -> ('a, 'b) M.t bindbox
  end


(** {2 Attributes of variables and utilities} *)


(** [prefix_of x] returns the [string] prefix of variable [x]'s name, which is
    only the first part of its full name (obtained with [name_of x]). The full
    name also contain an [int] suffix,  which is defined as the longest suffix
    of digits in the full name. *)
val prefix_of : 'a var -> string

(** [suffix_of x] returns the [int] suffix of variable [x]'s name. It consists
    in the longest digit suffix in the full name of [x] (see [prefix_of]). *)
val suffix_of : 'a var -> int

(** [hash_var x] computes a hash for variable [x]. Note that this function can
    be used with the [Hashtbl] module. *)
val hash_var  : 'a var -> int

(** [compare_vars x y] safely compares [x] and [y].  Note that it is unsafe to
    compare variables with [Pervasive.compare]. *)
val compare_vars : 'a var -> 'b var -> int

(** [eq_vars x y] safely computes the equality of [x] and [y]. Note that it is
    unsafe to compare variables with the polymorphic equality function. *)
val eq_vars : 'a var -> 'b var -> bool

(** [copy_var x name mkfree] makes a copy of variable [x],  with a potentially
    different name and [mkfree] function. However, the copy is treated exactly
    as the original in terms of binding and substitution. *)
val copy_var : 'b var -> string -> ('a var -> 'a) -> 'a var


(** {2 Attributes of binders and utilities} *)


(** [binder_name] returns the name of the variable bound by the [binder]. *)
val binder_name : ('a,'b) binder -> string

(** [binder_occur b] tests whether the bound variable occurs in [b]. *)
val binder_occur : ('a,'b) binder -> bool

(** [binder_constant b] tests whether the [binder] [b] is constant (i.e.,  its
    bound variable does not occur). *)
val binder_constant : ('a,'b) binder -> bool

(** [binder_closed b] test whether the [binder] [b] is closed (i.e.,  does not
    contain any free variable). *)
val binder_closed : ('a,'b) binder -> bool

(** [binder_rank b] gives the number of free variables contained in [b]. *)
val binder_rank : ('a,'b) binder -> int

(** [binder_compose_left f b] precomposes the binder [b] with the function [f]
    without changing anything at the binding structure. *)
val binder_compose_left : ('a -> 'b) -> ('b,'c) binder -> ('a,'c) binder

(** [binder_compose_rigth b f] postcomposes the binder [b] with  the  function
    [f] without changing anything at the binding structure. *)
val binder_compose_right : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder


(** [mbinder_arity b] gives the arity of the [mbinder]. *)
val mbinder_arity : ('a,'b) mbinder -> int

(** [mbinder_names b] return the array of the names of the variables bound  by
    the [mbinder] [b]. *)
val mbinder_names : ('a,'b) mbinder -> string array

(** [mbinder_occurs b] returns an array of [bool] indicating if the  variables
    that are bound occur (i.e., are used). *)
val mbinder_occurs : ('a,'b) mbinder -> bool array

(** [mbinder_constant b] indicates whether the [mbinder] [b] is constant. This
    means that none of its variables are used. *)
val mbinder_constant : ('a,'b) mbinder -> bool

(** [mbinder_closed b] indicates whether [b] is closed. *)
val mbinder_closed : ('a,'b) mbinder -> bool

(* [mbinder_rank b] gives the number of free variables contained in [b]. *)
val mbinder_rank : ('a,'b) mbinder -> int


(** {2 Attributes of bindboxes and utilities} *)


(** [is_closed b] checks whether the [bindbox] [b] is closed. *)
val is_closed : 'a bindbox -> bool

(** [occur x b] tells whether variable [x] occurs in the [bindbox] [b]. *)
val occur : 'a var -> 'b bindbox -> bool

(** [bind_apply bb barg] substitute the boxed binder [bb] with the boxed value
    [barb] in the [bindbox] type. This is useful for working with higher-order
    variables, which may be represented as binders themselves. *)
val bind_apply  : ('a, 'b) binder bindbox  -> 'a bindbox       -> 'b bindbox

(** [mbind_apply bb bargs] substitute the boxed binder  [bb]  with  the  boxed
    array of values [barbs] in the [bindbox] type.  This is useful for working
    with higher-order variables. *)
val mbind_apply : ('a, 'b) mbinder bindbox -> 'a array bindbox -> 'b bindbox

(** [fixpoint bb] constructs a binder fixpoint. This very advanced feature can
    be used to build recursive definitions (like with OCaml's "let rec"). *)
val fixpoint : (('a, 'b) binder, ('a, 'b) binder) binder bindbox
  -> ('a, 'b) binder bindbox


(** {2 Working in a context} *)


(** It is sometimes convenient to work in a context for variables, for example
    when one wishes to reserve variable names.  The [Bindlib] library provides
    a type of contexts, together with functions for creating variables and for
    binding variables in a context. *)

(** Type of a context. *)
type ctxt

(** [empty_ctxt] denotes the empty context. *)
val empty_ctxt : ctxt

(** [new_var_in ctxt mkfree name] is similar to [new_var mkfree name], but the
    variable names is chosen not to collide with the context [ctxt]. Note that
    the context that is returned contains the new variable name. *)
val new_var_in : ctxt -> ('a var -> 'a) -> string -> 'a var * ctxt

(** [new_mvar_in ctxt mkfree names] is similar to [new_mvar mkfree names], but
    it handles the context (see [new_var_in]). *)
val new_mvar_in : ctxt -> ('a var -> 'a) -> string array -> 'a mvar * ctxt

(** [unbind_in ctxt mkfree b] is similar to [unbind mkfree b],  but it handles
    the context (see [new_mvar_in]). *)
val unbind_in : ctxt -> ('a var -> 'a) -> ('a,'b) binder
  -> 'a var * 'b * ctxt

(** [munbind_in ctxt mkfree b] is like [munbind mkfree b],  but it handles the
    context (see [new_mvar_in]). *)
val unmbind_in : ctxt -> ('a var -> 'a) -> ('a,'b) mbinder
  -> 'a mvar * 'b * ctxt

(** [bind_in ctxt mkfree name f] is like [bind mkfree name f],  but it handles
    the context. *)
val bind_in : ctxt -> ('a var -> 'a) -> string
  -> ('a bindbox -> ctxt -> 'b bindbox) -> ('a,'b) binder bindbox

(** [mbind_in ctxt mkfree names f] is similar to  [mbind mkfree names f],  but
    it handles the context. *)
val mbind_in : ctxt -> ('a var -> 'a) -> string array
  -> ('a bindbox array -> ctxt -> 'b bindbox) -> ('a,'b) mbinder bindbox


(** {2 Unsafe, advanced features} *)


(** [reset_counter ()] resets the unique identifier counter on which [Bindlib]
    relies. This function should only be called when previously generated data
    (e.g., variables) cannot be accessed anymore. *)
val reset_counter : unit -> unit

(** [dummy_bindbox] can be used for initialising structires (e.g., arrays). If
    [unbox] is called on a data structure containing  a  [dummy_bindbox],  the
    exception [Failure "Invalid use of dummy_bindbox"] is raised. *)
val dummy_bindbox : 'a bindbox

(** [binder_from_fun name f] transform a function into a binder. Note that the
    function is only called when the binder is substituted (see [subst]). This
    is not the recommended way of building binders. Nonetheless,  it is useful
    for simple tasks such as contracting two binders into one, without copying
    the whole structure (e.g., transform \x.\y.t(x,y) into \x.t(x,x)). *)
val binder_from_fun : string -> ('a -> 'b) -> ('a,'b) binder

(** [mbinder_from_fun] is similar to [binder_from_fun]. *)
val mbinder_from_fun : string array -> ('a array -> 'b) -> ('a,'b) mbinder
