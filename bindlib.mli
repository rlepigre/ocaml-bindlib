(** The [Bindlib] library provides support for free and bound variables in the
    OCaml language. The main application is the construction of abstract types
    containing a binding structure (e.g., abstract syntax trees).

    @author Christophe Raffalli
    @author Rodolphe Lepigre *)


(** {2 Variables, binders and substitution} *)


(** The [Bindlib] library provides two type constructors for building abstract
    syntax trees: ['a var] and [('a,'b) binder]. Intuitively, ['a var] will be
    a representation for a free variable of type ['a],  and [('a,'b) binder] a
    represention for a term of type ['b] depending on a variable (or value) of
    type ['a] (the type [('a,'b) binder] can be seen as ['a -> 'b]). Note that
    types ['a mvar] and [('a,'b) mbinder] are provided for handling arrays  of
    variables. *)

(** Type of a free variable of type ['a]. *)
type 'a var

(** Type of an array of variables of type ['a]. *)
type 'a mvar = 'a var array

(** Type of a binder for an element of type ['a] into an element of type ['b].
    In terms of higher order abstract syntax, it can be seen as ['a -> 'b]. *)
type ('a,'b) binder

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
    the corresponding elements. It is a form of syntactic wrapper. Note that a
    variable name is understood as a couple of a prefix string, and a possible
    natural number suffix (the longest suffix of [name] formed of digits). For
    example, the variable name ["xzy"] will have no suffix, and ["xyz12"] will
    have the prefix ["xyz"] and the suffix [12]. Note that the name ["xyz007"]
    and ["xyz7"] are considered the same,  and are both shown as the latter by
    the [name_of] function. *)
val new_var  : ('a var -> 'a) -> string       -> 'a var

(** [new_mvar mkfree names] creates a new array of variables using a  function
    [mkfree] (see [new_var]) and  a [name]. *)
val new_mvar : ('a var -> 'a) -> string array -> 'a mvar

(** Following on our example of the lambda-calculus, the [mkfree] function for
    variables of type [term var] could be defined as follows. {[
    let mkfree : term var -> term = fun x -> Var(x) ]} *)

(** [name_of x] returns a printable name for variable [x]. *)
val name_of  : 'a var  -> string

(** [names_of xs] returns printable names for the variables of [xs]. *)
val names_of : 'a mvar -> string array

(** [unbind b] breaks the binder [b] into a pair of a variable and a body. The
    name of the variable is based on that of the binder. *)
val unbind : ('a,'b) binder -> 'a var * 'b

(** [unbind2 f g] is similar to [unbind f], but it substitutes two binders [f]
    and [g] at once, using the same fresh variable. *)
val unbind2 : ('a,'b) binder -> ('a,'c) binder -> 'a var * 'b * 'c

(** [eq_binder eq f g] tests the equality between [f] and [g]. The binders are
    first substituted with the same fresh variable,  and [eq] is called on the
    resulting terms. *)
val eq_binder : ('b -> 'b -> bool) -> ('a,'b) binder -> ('a,'b) binder -> bool

(** [unmbind b] breaks down the binder [b] into an array of variables, and the
    term in which these variables are now free. The name of the variables that
    are created are based on the bound variable names. *)
val unmbind : ('a,'b) mbinder -> 'a mvar * 'b

(** [unmbind2 f g] is similar to [unmbind f],  but it substitutes both [f] and
    [g] using the same fresh variables. *)
val unmbind2 : ('a,'b) mbinder -> ('a,'c) mbinder -> 'a mvar * 'b * 'c

(** [eq_mbinder eq f g] tests the equality between two [mbinder] [f] and  [g].
    They are first substituted with the same fresh variables, and then [eq] is
    called on the resulting terms. *)
val eq_mbinder : ('b -> 'b -> bool) -> ('a,'b) mbinder -> ('a,'b) mbinder
  -> bool

(** An usual use of [unbind] is the wrinting of pretty-printing functions. The
    function given bellow transforms a lambda-term into a [string].  Note that
    the [name_of] function is used for variables. {[
    let rec to_string : term -> string = fun t ->
      match t with
      | Var(x)   -> name_of x
      | Abs(b)   -> let (x,t) = unbind b in
                    "\\" ^ name_of x ^ "." ^ to_string t
      | App(t,u) -> "(" ^ to_string t ^ ") " ^ to_string u ]} *)


(** {2 Constructing terms and binders in the binding box} *)


(** One of the main design priciple of the [Bindlib] library is efficiency. To
    obtain very fast substitutions, a price is paid at the construction of the
    terms. Indeed,  binders (i.e., element of type [('a,'b) binder]) cannot be
    defined directly. Instead, they are put together in the type ['a box].  It
    correspond to a term of type ['a] which free variables may be bound in the
    future. *)

(** Type of a term of type ['a] under construction. Using this representation,
    the free variable of the term can be bound easily. *)
type +'a box

(** [box_var x] builds a ['a box] from the ['a var] [x]. *)
val box_var : 'a var -> 'a box

(** [box e] injects the value [e] into the ['a box] type,  assuming that it is
    closed. Thus, if [e] contains variables,  then they will not be considered
    free. This means that no variable of [e] will be available for binding. *)
val box : 'a -> 'a box

(** [apply_box bf ba] applies the boxed function [bf] to a boxed argument [ba]
    inside the ['a box] type. This function is used to buld new expressions by
    applying a function with free variables to an argument with free variables
    (the ['a box] type is an applicative functor which application operator is
    [apply_box], and which unit is [box]). *)
val apply_box : ('a -> 'b) box -> 'a box -> 'b box

(** [box_apply f ba] applies the function [f] to a boxed argument [ba].  It is
    equivalent to [apply_box (box f) ba], but is more efficient. *)
val box_apply : ('a -> 'b) -> 'a box -> 'b box

(** [box_apply2 f ba bb] applies the function [f] to two boxed arguments  [ba]
    and [bb]. It is equivalent to [apply_box (apply_box (box f) ba) bb] but it
    is more efficient. *)
val box_apply2 : ('a -> 'b -> 'c) -> 'a box -> 'b box -> 'c box

(** [bind_var x b] binds the variable [x] in [b] to produce a boxed binder. In
    fact, is used to implement [bind] and [vbind]. *)
val bind_var  : 'a var  -> 'b box -> ('a, 'b) binder box

(** [bind_mvar xs b] binds the variables of [xs] in [b] to get a boxed binder.
    In fact, [bind_mvar] is used to implement [mbind] and [mvbind]. *)
val bind_mvar : 'a mvar -> 'b box -> ('a, 'b) mbinder box

(** [box_binder f b] boxes the binder [b] using the boxing function [f].  Note
    that when [b] is closed, it is immediately boxed using the [box] function.
    In that case, the function [f] is not used at all. *)
val box_binder : ('b -> 'b box) -> ('a,'b) binder -> ('a,'b) binder box

(** [box_mbinder f b] boxes the multiple binder [b] using the boxings function
    [f]. Note that if [b] is closed then it is immediately boxed (with [box]),
    without relying on [f] at all. *)
val box_mbinder : ('b -> 'b box) -> ('a,'b) mbinder -> ('a,'b) mbinder box

(** As mentioned earlier,  terms with bound variables can only be built in the
    ['a box] type. To ease the construction of terms, it is a good practice to
    implement “smart constructors” at the ['a box] level.  Coming back to  our
    λ-calculus example, we can give the following smart constructors. {[
    let var : term var -> term box =
      fun x -> box_var x

    let abs : term var -> term box -> term box =
      fun x t -> box_apply (fun b -> Abs(b)) (bind_var x t)

    let app : term box -> term box -> term box =
      fun t u -> box_apply2 (fun t u -> App(t,u)) t u ]} *)

(** [unbox e] can be called when the construction of a term is finished (e.g.,
    when the desired variables have all been bound). *)
val unbox : 'a box -> 'a

(** We can then easily define terms of the lambda-calculus as follows. {[
    let id    : term = (* \x.x *)
      let x = new_var "x" mkfree in
      unbox (abs x (var x))

    let fst   : term = (* \x.\y.x *)
      let x = new_var "x" mkfree in
      let y = new_var "y" mkfree in
      unbox (abs x (abs y (var x)))

    let omega : term = (* (\x.(x) x) \x.(x) x *)
      let x = new_var "x" mkfree in
      let delta = abs x (app (var x) (var x)) in
      unbox (app delta delta) ]} *)


(** {2 More binding box manipulation functions} *)


(** In general, it is not difficult to use the [box] and [apply_box] functions
    to manipulate any kind of data in the ['a box] type. However, working with
    these functions alone can be tedious.  The following functions can be used
    to manipulate standard data types in an optimised way. *)

(** [box_opt bo] shifts the [option] type of [bo] into the [box]. *)
val box_opt : 'a box option -> 'a option box

(** [box_list bs] shifts the [list] type of [bs] into the [box]. *)
val box_list : 'a box list  -> 'a list  box

(** [box_rev_list bs] is similar to [box_list bs], but the produced boxed list
    is reversed (it is hence more efficient). *)
val box_rev_list : 'a box list  -> 'a list  box

(** [box_array bs] shifts the [array] type of [bs] into the [box]. *)
val box_array : 'a box array -> 'a array box

(** [box_apply3] is similar to [box_apply2]. *)
val box_apply3 : ('a -> 'b -> 'c -> 'd)
  -> 'a box -> 'b box -> 'c box -> 'd box

(** [box_apply4] is similar to [box_apply2] and [box_apply3]. *)
val box_apply4 : ('a -> 'b -> 'c -> 'd -> 'e)
  -> 'a box -> 'b box -> 'c box -> 'd box -> 'e box

(** [box_pair ba bb] is the same as [box_apply2 (fun a b -> (a,b)) ba bb]. *)
val box_pair : 'a box -> 'b box -> ('a * 'b) box

(** [box_trible] is similar to [box_pair], but for triples. *)
val box_triple : 'a box -> 'b box -> 'c box -> ('a * 'b * 'c) box

(** Type of a module equipped with a [map] function. *)
module type Map =
  sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

(** Functorial interface used to build lifting functions for any type equipped
    with a [map] function. In other words,  this function can be used to allow
    the permutation of the ['a box] type with another type constructor. *)
module Lift(M : Map) :
  sig
    val lift_box : 'a box M.t -> 'a M.t box
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
    val lift_box : ('a box, 'b box) M.t -> ('a, 'b) M.t box
  end


(** {2 Attributes of variables and utilities} *)


(** [hash_var x] computes a hash for variable [x]. Note that this function can
    be used with the [Hashtbl] module. *)
val hash_var  : 'a var -> int

(** [compare_vars x y] safely compares [x] and [y].  Note that it is unsafe to
    compare variables with [Pervasive.compare]. *)
val compare_vars : 'a var -> 'b var -> int

(** [eq_vars x y] safely computes the equality of [x] and [y]. Note that it is
    unsafe to compare variables with the polymorphic equality function. *)
val eq_vars : 'a var -> 'b var -> bool


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


(** {2 Attributes of binding boxes and utilities} *)


(** [is_closed b] checks whether the [box] [b] is closed. *)
val is_closed : 'a box -> bool

(** [is_subst b] checks whether the [box] [b] was substituted. *)
val is_substituted : (bool -> 'a) box -> 'a box

(** [occur x b] tells whether variable [x] occurs in the [box] [b]. *)
val occur : 'a var -> 'b box -> bool

(** [bind_apply bb barg] substitute the boxed binder [bb] with the boxed value
    [barb] in the [box] type. This function is useful when working with higher
    order variables, which may be represented as binders themselves. *)
val bind_apply : ('a, 'b) binder box -> 'a box -> 'b box

(** [mbind_apply bb bargs] substitute the boxed binder [bb] with a boxed array
    of values [barbs] in the [box] type.  This function is useful when working
    with higher order variables. *)
val mbind_apply : ('a, 'b) mbinder box -> 'a array box -> 'b box


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

(** [unbind_in ctxt b] is similar to [unbind b], but it handles the context as
    explained in the documentation of [new_mvar_in]. *)
val unbind_in : ctxt -> ('a,'b) binder -> 'a var * 'b * ctxt

(** [unmbind_in ctxt b] is like [unmbind b],  but it handles the context as is
    explained in the documentation of [new_mvar_in]. *)
val unmbind_in : ctxt -> ('a,'b) mbinder -> 'a mvar * 'b * ctxt


(** {2 Unsafe, advanced features} *)


(** [uid_of x] returns a unique identifier of the given variable. *)
val uid_of  : 'a var  -> int

(** [uids_of xs] returns the unique identifiers of the variables of [xs]. *)
val uids_of : 'a mvar -> int array

(** [copy_var x name mkfree] makes a copy of variable [x],  with a potentially
    different name and [mkfree] function. However, the copy is treated exactly
    as the original in terms of binding and substitution. The main application
    of this function is for translating abstract syntax trees while preserving
    binders. In particular, variables at two different types should never live
    together (this may produce segmentation faults). *)
val copy_var : 'b var -> string -> ('a var -> 'a) -> 'a var

(** [reset_counter ()] resets the unique identifier counter on which [Bindlib]
    relies. This function should only be called when previously generated data
    (e.g., variables) cannot be accessed anymore. *)
val reset_counter : unit -> unit

(** [dummy_box] can be used for initialising structures like arrays. Note that
    if [unbox] is called on a data structure containing [dummy_box],  then the
    exception [Failure "Invalid use of dummy_box"] is raised. *)
val dummy_box : 'a box

(** [binder_compose b f] postcomposes the binder [b] with the function [f]. In
    the process, the binding structure is not changed. Note that this function
    is not alwasy safe. Use it with care. *)
val binder_compose : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder

(** [mbinder_compose b f] postcomposes the multiple binder [b] with [f]. This
    function is similar to [binder_compose], and it is not always safe. *)
val mbinder_compose : ('a,'b) mbinder -> ('b -> 'c) -> ('a,'c) mbinder

(** [raw_binder name bind rank mkfree value] builds a binder using the [value]
    function as its definition. The parameter [name] correspond to a preferred
    name of the bound variable, the boolean [bind] indicates whether the bound
    variable occurs, and [rank] gives the number of distinct free variables in
    the produced binder. The [mkfree] function injecting variables in the type
    ['a] of the domain of the binder must also be given. This function must be
    considered unsafe because it is the responsibility of the user to give the
    accurate value for [bind] and [rank]. *)
val raw_binder : string -> bool -> int -> ('a var -> 'a)
  -> ('a -> 'b) -> ('a,'b) binder

(** [raw_mbinder names binds rank value] is similar to [raw_binder], but it is
    applied to a multiple binder. As for [raw_binder], this function has to be
    considered unsafe because the user must enforce invariants. *)
val raw_mbinder : string array -> bool array -> int -> ('a var -> 'a)
  -> ('a array -> 'b) -> ('a,'b) mbinder
