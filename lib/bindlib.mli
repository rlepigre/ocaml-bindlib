(** The [Bindlib] library provides support for free and bound variables in the
    OCaml language. The main application is the representation of types with a
    binding structure (e.g., abstract syntax trees).

    @author Christophe Raffalli
    @author Rodolphe Lepigre
    @version 6.0 *)


(** {2 Variables, binders and substitution} *)


(** The [Bindlib] library provides two type constructors for building abstract
    syntax trees: type ['a var] representing a free variable of type ['a], and
    type [('a,'b) binder] representing a binding of a variable of type ['a] in
    a value of type ['b]. Intuitively, type [('a,'b) binder] can be thought of
    as ['a -> 'b]. Types ['a mvar] and [('a,'b) mbinder] are also provided for
    handling arrays of variables more efficiently. *)

(** Type of a free variable of type ['a]. *)
type 'a var

(** Type of an array of variables of type ['a]. *)
type 'a mvar = 'a var array

(** Type of a binder for a variable of type ['a] into an element of type ['b].
    In terms of higher order abstract syntax, it can be seen as ['a -> 'b]. *)
type ('a,'b) binder

(** Type of a binder for an array of variables of type ['a] into an element of
    type ['b]. This representation is more efficient than a nesting of binders
    of type [('a,'b) binder], but substitution can only be performed the whole
    array of variables at once. *)
type ('a,'b) mbinder

(** As an example, a type representing the terms of the pure λ-calculus can be
    defined as follows using types ['a var] and [('a,'b) binder].
    {[ type term =
         | Var of term var
         | Abs of (term, term) binder
         | App of term * term ]} *)

(** [subst b v] substitutes the variable bound by [b] with the value [v]. This
    operation is very efficient. *)
val subst : ('a,'b) binder -> 'a -> 'b

(** [msubst b vs] substitutes the variables bound by [b] with the values [vs].
    This operation is very efficient. Note however that the length of the [vs]
    array should match the arity of [b] (as given by [mbinder_arity b]). If it
    is not the case, the exception [Invalid_argument "Bad arity in msubst"] is
    raised. *)
val msubst : ('a,'b) mbinder -> 'a array -> 'b

(** Coming back to our pure λ-calculus example, call-by-name evaluation can be
    defined as a simple recursive function using [subst].
    {[ let rec eval : term -> term = fun t ->
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
    the corresponding objects: it is a form of syntactic wrapper that is often
    defined to be a constructor (see the example below). *)
val new_var : ('a var -> 'a) -> string -> 'a var

(** [new_mvar mkfree names] creates an array of new variables using a function
    [mkfree] (see [new_var]) and an array of variable [names]. *)
val new_mvar : ('a var -> 'a) -> string array -> 'a mvar

(** For our pure λ-calculus example, where variables have type [term var], the
    [mkfree] function would simply be defined as follows.
    {[ let mkfree : term var -> term = fun x -> Var(x) ]} *)

(** [name_of x] returns the name corresponding to variable [x]. Note that this
    name is generally not safe for printing since names are not updated when a
    substitution is performed. For instance, variables may appear to have been
    captured in the text representation of a structure with variable bindings.
    To circumvent this issue, one must be particularly careful when converting
    binders into text, and rely on contexts (see type [ctxt] below). *)
val name_of : 'a var -> string

(** [names_of xs] returns names corresponding to variables [xs]. As when using
    [name_of], care should be taken when converting objects with bindings into
    a text representation. *)
val names_of : 'a mvar -> string array

(** [unbind b] substitutes the binder [b] using a fresh variable. The variable
    and the result of the substitution are returned. Note that the name of the
    fresh variable is based on that of the binder, but it is not guaranteed to
    be safe for printing (see [new_var] and [unbind_in]). However, there is no
    problem for other operations such as re-establishing the binding. When the
    fresh variable is created, the [mkfree] function that is used is that that
    was specified when creating the variable that was originally bound by [b],
    at the time of its creation (see [new_var] and [bind_var]). *)
val unbind : ('a,'b) binder -> 'a var * 'b

(** [unbind2 f g] is similar to [unbind f], but it substitutes two binders [f]
    and [g] at once using the same fresh variable. The name of the variable is
    based on that of the binder [f]. Similarly, the [mkfree] syntactic wrapper
    that is used for the fresh variable is the one that was given for creating
    the variable that was bound to construct [f] (see [bind_var] and [new_var]
    for details on this process). In particular, the use of [unbind2] may lead
    to unexpected results if the binders [f] and [g] were not built using free
    variables created with the same [mkfree].  Moreover, as with [unbind], the
    name of the fresh variable is not guaranteed to be safe for printing. *)
val unbind2 : ('a,'b) binder -> ('a,'c) binder -> 'a var * 'b * 'c

(** [eq_binder eq f g] tests the equality between [f] and [g]. The binders are
    first substituted with the same fresh variable (using [unbind2]), and [eq]
    is called on the resulting values.  Note that [eq_binder] may not have the
    expected result if [f] and [g] were not built by binding variables with an
    identical [mkfree] syntactic wrapper. *)
val eq_binder : ('b -> 'b -> bool) -> ('a,'b) binder -> ('a,'b) binder -> bool

(** [unmbind b] substitutes the multiple binder [b] with fresh variables. This
    function is analogous to [unbind] for binders. Note that the names used to
    create the fresh variables are based on those of the multiple binder.  The
    syntactic wrapper (of [mkfree]) that is used to build the variables is the
    one that was given when creating the multiple variables that were bound in
    [b] (see [new_mvar] and [bind_mvar]). Moreover, note that as for [unbind],
    the names of the fresh variables may not be safe for printing. *)
val unmbind : ('a,'b) mbinder -> 'a mvar * 'b

(** [unmbind2 f g] is similar to [unmbind f],  but it substitutes two multiple
    binder [f] and [g] at once, using the same fresh variables.  Note that the
    two binders must have the same arity. This function may have an unexpected
    results in some cases, and the fresh variables may have names that are not
    safe for printing (see [unbind2]). *)
val unmbind2 : ('a,'b) mbinder -> ('a,'c) mbinder -> 'a mvar * 'b * 'c

(** [eq_mbinder eq f g] tests the equality of the two multiple binders [f] and
    [g]. They are substituted with the same fresh variables (using [unmbind2])
    and [eq] is called on the resulting values. This function may not have the
    expected result in some cases,  for reasons explained in the documentation
    of [eq_binder]. It is safe to use this function on multiple binders with a
    different arity (they are considered different). *)
val eq_mbinder : ('b -> 'b -> bool) -> ('a,'b) mbinder -> ('a,'b) mbinder
  -> bool

(** The [unbind] function is often useful for term traversals. For instance, a
    function computing the size of a pure λ-term can be defined as follows.
    {[ let rec size : term -> int = fun t ->
         match t with
         | Var(_)   -> 0
         | Abs(b)   -> let (_,t) = unbind b in
                       1 + size t
         | App(t,u) -> 1 + size t + size u ]}
    Note however that it is not a good idea to define a term-printing function
    using a combination of [unbind] and [name_of]. Indeed, as explained in the
    documentation of [name_of], contexts must be used to ensure that displayed
    variable names are correct. *)


(** {2 Constructing terms and binders in the binding box} *)


(** To obtain fast substitutions,  a price must be paid at the construction of
    terms. Indeed,  binders (i.e., element of type [('a,'b) binder]) cannot be
    defined directly. Instead, they are put together in the type ['a box].  It
    correspond to a term of type ['a] whose free variables may be bound. *)

(** Type of a term of type ['a] under construction. Using this representation,
    the free variables of the term can be bound easily. *)
type +'a box

(** [box_var x] injects variable [x] into the ['a box] type, so that it can be
    bound using [bind_var]. *)
val box_var : 'a var -> 'a box

(** [box e] injects the value [e] into the ['a box] type,  assuming that it is
    closed. Thus, if [e] contains variables,  then they will not be considered
    free. This means that no variable of [e] will be available for binding. *)
val box : 'a -> 'a box

(** [apply_box bf ba] applies the boxed function [bf] to a boxed argument [ba]
    inside the [box] type.  This function is used to build new expressions  by
    applying a function with free variables to an argument with free variables
    (the ['a box] type is an applicative functor whose application operator is
    [apply_box], and whose unit is [box]). *)
val apply_box : ('a -> 'b) box -> 'a box -> 'b box

(** [box_apply f ba] applies the function [f] to a boxed argument [ba].  It is
    equivalent to [apply_box (box f) ba], but is more efficient. *)
val box_apply : ('a -> 'b) -> 'a box -> 'b box

(** [box_apply2 f ba bb] applies the function [f] to two boxed arguments: [ba]
    and [bb]. It is equivalent to [apply_box (apply_box (box f) ba) bb] but it
    is more efficient. *)
val box_apply2 : ('a -> 'b -> 'c) -> 'a box -> 'b box -> 'c box

(** [bind_var x b] binds the variable [x] in [b], producing a boxed binder. *)
val bind_var : 'a var -> 'b box -> ('a, 'b) binder box

(** [bind_mvar xs b] binds the variables of [xs] in [b] to get a boxed binder.
    It is the equivalent of [bind_var] to build a multiple binder. *)
val bind_mvar : 'a mvar -> 'b box -> ('a, 'b) mbinder box

(** [box_binder f b] boxes the binder [b] using the boxing function [f].  Note
    that when [b] is closed, it is immediately boxed using the [box] function.
    In that case, the function [f] is not used at all. *)
val box_binder : ('b -> 'b box) -> ('a,'b) binder -> ('a,'b) binder box

(** [box_mbinder f b] boxes the multiple binder [b] using the boxing  function
    [f]. Note that if [b] is closed then it is immediately boxed (with [box]),
    without relying on [f] at all. *)
val box_mbinder : ('b -> 'b box) -> ('a,'b) mbinder -> ('a,'b) mbinder box

(** As mentioned earlier,  terms with bound variables can only be built in the
    ['a box] type. To ease the construction of terms, it is a good practice to
    implement “smart constructors” at the ['a box] level.  Coming back to  our
    λ-calculus example, we can give the following smart constructors.
    {[ let var : term var -> term box =
         fun x -> box_var x

       let abs_raw : (term, term) binder box -> term box =
         fun b -> box_apply (fun b -> Abs(b)) b

       let abs : term var -> term box -> term box =
         fun x t -> abs_raw (bind_var x t)

       let app : term box -> term box -> term box =
         fun t u -> box_apply2 (fun t u -> App(t,u)) t u ]}
    Additionally, it is a good idea to define a “boxing function”:  a function
    used to turn terms into a boxed terms. It is sometimes necessary to do so,
    as we will see when we define a printing function.
    {[ let rec box_term : term -> term box = fun t ->
         match t with
         | Var(x)   -> var x
         | Abs(b)   -> abs_raw (box_binder box_term b)
         | App(t,u) -> app (box_term t) (box_term u) ]} *)

(** [unbox e] can be called when the construction of a term is finished (i.e.,
    when the desired variable bindings have been created). *)
val unbox : 'a box -> 'a

(** We can then easily define terms of the λ-calculus as follows.
    {[ (* λx.x *)
       let id : term =
         let x = new_var "x" mkfree in
         unbox (abs x (var x))

       (* λx.λy.x *)
       let fst   : term =
         let x = new_var "x" mkfree in
         let y = new_var "y" mkfree in
         unbox (abs x (abs y (var x)))

       (* λx.(x) x) (boxed) *)
       let delta : term box =
         let x = new_var "x" mkfree in
         abs x (app (var x) (var x))

       (* (λx.(x) x) λx.(x) x *)
       let omega : term =
         unbox (app delta delta)

       (* λx.(x) x) *)
       let delta : term =
         unbox delta ]} *)


(** {2 Working in a context and variable printing} *)


(** For variable substitution to be as fast as possible, the [Bindlib] library
    does not do any work to maintain variable names at substitution time. This
    work is instead delayed until it becomes necessary: at the time of turning
    objects with binders into a textual representation (e.g., for printing the
    result of a computation). Such operations must hence maintain a context of
    variable names using the functions of this section. *)

(** Type of a context. *)
type ctxt

(** [empty_ctxt] denotes the empty context. *)
val empty_ctxt : ctxt

(** [free_vars b] returns a context containing the free variables of [b]. *)
val free_vars : 'a box -> ctxt

(** [reserve_name name ctxt] extends context [ctxt] by reserving variable name
    [name]. *)
val reserve_name : string -> ctxt -> ctxt

(** [new_var_in ctxt mkfree name] is similar to [new_var mkfree name], but the
    name actually used for the newly created variable is chosen to not collide
    with the variables of context [ctxt]. Said otherwise, argument [name] only
    gives a preferred name: if it is not available then a fresh name is picked
    by appending a decimal number at the end of [name]. Moreover, the obtained
    variable has a name that is safe for printing (see [name_of]), at least as
    long as binders are not substituted in objects containing it. Finally, the
    context that is returned is extended to contain the new variable name. *)
val new_var_in : ctxt -> ('a var -> 'a) -> string -> 'a var * ctxt

(** [new_mvar_in ctxt mkfree names] is similar to [new_mvar mkfree names], but
    it handles renaming based on context [ctxt] (see [new_var_in]). *)
val new_mvar_in : ctxt -> ('a var -> 'a) -> string array -> 'a mvar * ctxt

(** [unbind_in ctxt b] is similar to [unbind b], but it handles the context as
    explained in the documentation of [new_mvar_in]. This function can be used
    for maintaining correct names in printing functions: it is safe to use the
    [name_of] function on the returned variable (as long as no substitution is
    performed in the involved objects, see [new_var_in]). *)
val unbind_in : ctxt -> ('a,'b) binder -> 'a var * 'b * ctxt

(** [unbind2_in ctxt f g] is similar to [unbind2 f g], but handles the context
    as explained in the documentation of [new_mvar_in] and [unbind_in]. *)
val unbind2_in : ctxt -> ('a,'b) binder -> ('a,'c) binder
  -> 'a var * 'b * 'c * ctxt

(** [unmbind_in ctxt b] is similar to [unmbind b],  but it handles the context
    as is explained in the documentation of [new_mvar_in]. As [unbind_in], the
    [unmbind_in] function can be used to implement printing functions. *)
val unmbind_in : ctxt -> ('a,'b) mbinder -> 'a mvar * 'b * ctxt

(** [unmbind2_in ctxt f g] is similar to [unmbind2 f g], but it uses a context
    similrly to [new_mvar_in] or [unmbind_in]. *)
val unmbind2_in : ctxt -> ('a,'b) mbinder -> ('a,'c) mbinder
  -> 'a mvar * 'b * 'c * ctxt

(** Going back to our λ-calculus example, the [unbind_in] function can be used
    to implement the following function transforming a λ-term into a [string].
    Here, thanks to the use of [unbind_in], it is safe to rely on [name_of] to
    print variables.
    {[ let to_string : ctxt -> term -> string = fun ctxt t ->
         match t with
         | Var(x)   -> name_of x
         | Abs(b)   -> let (x,t,ctxt) = unbind_in ctxt b in
                       "λ" ^ name_of x ^ "." ^ to_string ctxt t
         | App(t,u) -> "(" ^ to_string ctxt t ^ ") " ^ to_string ctxt u ]}
    The above [to_string] must not only receive the term to display (i.e., the
    second argument),  but also a context containing all free variables in the
    term. To avoid maintain such a context we can rely on function [free_vars]
    together with the term boxing function [box_term] defined earlier.
    {[ let to_string : term -> term = fun t ->
         to_string (free_vars (box_term t)) t ]} *)


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

(** [box_triple] is similar to [box_pair], but for triples. *)
val box_triple : 'a box -> 'b box -> 'c box -> ('a * 'b * 'c) box

(** Type of a module equipped with a [map] function. *)
module type Map = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** Functorial interface used to build lifting functions for any type equipped
    with a [map] function. In other words,  this function can be used to allow
    the permutation of the ['a box] type with another type constructor. *)
module Lift(M : Map) : sig
  val lift_box : 'a box M.t -> 'a M.t box
end

(** Type of a module equipped with a "binary" [map] function. *)
module type Map2 = sig
  type ('a, 'b) t
  val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
end

(** Similar to the [Lift] functor, but handles "binary" [map] functions. *)
module Lift2(M : Map2) : sig
  val lift_box : ('a box, 'b box) M.t -> ('a, 'b) M.t box
end


(** {2 Attributes of variables and utilities} *)


(** [hash_var x] computes a hash for variable [x]. Note that this function can
    be used with the [Hashtbl] module. *)
val hash_var : 'a var -> int

(** [compare_vars x y] safely compares [x] and [y].  Note that it is unsafe to
    compare variables using [Stdlib.compare]. *)
val compare_vars : 'a var -> 'b var -> int

(** [eq_vars x y] safely computes the equality of [x] and [y]. Note that it is
    unsafe to compare variables with [Stdlib.(=)]. *)
val eq_vars : 'a var -> 'b var -> bool


(** {2 Attributes of binders and utilities} *)


(** [binder_name b] returns the name of the variable bound by binder [b]. This
    name is generally not safe for printing, since it is not updated after the
    binder is created, even when a substitution is performed  (see [unbind_in]
    and [name_of] to learn how to print terms with binders). *)
val binder_name : ('a,'b) binder -> string

(** [binder_occur b] returns a boolean indicating if the variable bound by [b]
    occurs (i.e., is used). This is a constant time operation. *)
val binder_occur : ('a,'b) binder -> bool

(** [binder_constant b] is the same as [not (binder_occur b)]. *)
val binder_constant : ('a,'b) binder -> bool

(** [binder_closed b] indicates whether the [b] is closed (i.e., does not have
    any free variables). This is a constant time operation. *)
val binder_closed : ('a,'b) binder -> bool

(** [binder_rank b] gives the number of free variables contained in [b].  This
    is a constant time operation. *)
val binder_rank : ('a,'b) binder -> int

(** [mbinder_arity b] gives the arity of [b] (i.e., the number of variables it
    is binding). This is a constant time operation. *)
val mbinder_arity : ('a,'b) mbinder -> int

(** [mbinder_names b] returns the names of the variables bound by the multiple
    binder [b] as an array. Similarly the result of [binder_name], these names
    are not generally safe for printing. *)
val mbinder_names : ('a,'b) mbinder -> string array

(** [mbinder_occurs b] returns an array of booleans indicating whether each of
    the variables that are bound occur (i.e., are used). It is a constant time
    operation. *)
val mbinder_occurs : ('a,'b) mbinder -> bool array

(** [mbinder_constant b] indicates whether the [mbinder] [b] is constant. This
    means that none of its variables are used. *)
val mbinder_constant : ('a,'b) mbinder -> bool

(** [mbinder_closed b] indicates whether the multiple binder [b] is closed. It
    is a constant time operation. *)
val mbinder_closed : ('a,'b) mbinder -> bool

(** [mbinder_rank b] gives the number of free variables contained in [b]. This
    is a constant time operation. *)
val mbinder_rank : ('a,'b) mbinder -> int


(** {2 Attributes of binding boxes and utilities} *)


(** [is_closed b] checks whether [b] is closed (in constant time). *)
val is_closed : 'a box -> bool

(** [occur x b] indicates whether variable [x] occurs in [b].  This is done in
    linear time with respect to the number of free variables in [b]. *)
val occur : 'a var -> 'b box -> bool

(** [bind_apply bb barg] is the same as [box_apply2 subst bb barg]. *)
val bind_apply : ('a, 'b) binder box -> 'a box -> 'b box

(** [mbind_apply bb bargs] is the same as [box_apply2 msubst bb bargs]. *)
val mbind_apply : ('a, 'b) mbinder box -> 'a array box -> 'b box


(** {2 Custom context and variable renaming} *)


(** The variable renaming performed by functions like [new_var_in] is somewhat
    arbitrary, and may not fit every application. As a consequence, we provide
    a functor [Ctxt] that can be used to define a new renaming policy based on
    a custom notion of context and several configuration options. *)

(** Module type giving the specification of a renaming policy, to be used with
    the [Ctxt] functor. *)
module type Renaming = sig
  (** Type representing a set of variable names. *)
  type ctxt

  (** [empty_ctxt] represents an empty context. *)
  val empty_ctxt : ctxt

  (** [reset_context_for_closed_terms] indicates whether the context should be
      reset to the empty context when calling [unbind_in] or [munbind_in] on a
      closed binder (which cannot capture names). If set to [true], printing a
      λ-term will produce “λx.λx.x” rather than “λx.λx0.x0”,  or “λx.x (λx.x)”
      rather than “λx.x (λx0.x0)”. *)
  val reset_context_for_closed_terms : bool

  (** [skip_constant_binders] indicates whether binders that are constant must
      be skipped (i.e., not recorded in the context). This permits reusing the
      name in a lower binder like in “λx.λx.x”, but not in “λx.x (λx.x)”. *)
  val skip_constant_binders : bool

  (** [constant_binder_name] optionally provides a representation for a binder
      that is constant, to highlight the absence of a name. When this value is
      defined to be [Some(s)],  then [s] is used as name for all such binders.
      For example, if [s] is "_" then we would get “λ_.λx.x”. Note that if the
      value of [constant_binder_name] is not [None], [skip_constant_binder] is
      ignored. *)
  val constant_binder_name : string option

  (** [new_name name ctxt] creates a name that is fresh in context [ctxt]. The
      given [name] indicates a prefered name (or base for the name). Note that
      the returned context extends [ctxt] with the new name. *)
  val new_name : string -> ctxt -> string * ctxt

  (** [reserve_name name ctxt] extends context [ctxt] by reserving [name] as a
      free variable name. *)
  val reserve_name : string -> ctxt -> ctxt
end

(** The renaming policy used by the default context-manipulating function like
    [new_var_in] or [unbind_in] uses the following configuration: the value of
    [reset_context_for_closed_terms = false], [skip_constant_binders = false],
    and [constant_binder_name = None].

    The default [new_name] function is not exposed in the interface. It splits
    the [name] into a pair [(prefix,n)] containing the largest [n] and longest
    [prefix] such that [name = prefix ^ string_of_int n].  For instance, if we
    have [name = "xy023"] then [(prefix,n) = ("xy0",23)]. In the case where no
    [n] exists, we take [(prefix,n) = (name,0)].  The prefix is preserved, and
    only the value of [n] is changed for renaming. *)

(** A functor that can be used to obtain context-manipulating functions, given
    the specification of a renaming policy. The defined [ctxt] type as well as
    the obtained functions can then be used as a drop-in replacement for their
    default counterparts (found at the top level of the [Bindlib] module). *)
module Ctxt(R:Renaming) : sig
  type ctxt = R.ctxt

  val empty_ctxt : ctxt

  val free_vars : 'a box -> ctxt

  val new_var_in : ctxt -> ('a var -> 'a) -> string -> 'a var * ctxt

  val new_mvar_in : ctxt -> ('a var -> 'a) -> string array -> 'a mvar * ctxt

  val unbind_in : ctxt -> ('a,'b) binder -> 'a var * 'b * ctxt

  val unbind2_in : ctxt -> ('a,'b) binder -> ('a,'c) binder
    -> 'a var * 'b * 'c * ctxt

  val unmbind_in : ctxt -> ('a,'b) mbinder -> 'a mvar * 'b * ctxt

  val unmbind2_in : ctxt -> ('a,'b) mbinder -> ('a,'c) mbinder
    -> 'a mvar * 'b * 'c * ctxt
end


(** {2 Unsafe, advanced features} *)


(** [uid_of x] returns the unique identifier of the given variable. *)
val uid_of : 'a var  -> int

(** [uids_of xs] returns the unique identifiers of the variables of [xs]. *)
val uids_of : 'a mvar -> int array

(** [copy_var x mkfree name] makes a copy of variable [x],  with a potentially
    different name and [mkfree] function. However, the copy is treated exactly
    as the original in terms of binding and substitution. The main application
    of this function is for translating abstract syntax trees while preserving
    binders. In particular, variables at two different types should never live
    together (this may produce segmentation faults). *)
val copy_var : 'b var -> ('a var -> 'a) -> string -> 'a var

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
    is not always safe. Use it with care. *)
val binder_compose : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder

(** [mbinder_compose b f] postcomposes the multiple binder [b] with [f].  This
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

(** [raw_mbinder names binds rank mk_free value] is similar  to  [raw_binder],
    but it is applied to a multiple binder. As for [raw_binder], this function
    has to be considered unsafe because the user must enforce invariants. *)
val raw_mbinder : string array -> bool array -> int -> ('a var -> 'a)
  -> ('a array -> 'b) -> ('a,'b) mbinder
