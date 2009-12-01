(**
Bindlib library
*)

(** {2 Basic functions} *)
(** this is the type of an expression of type ['b] with a bound variables of 
 type ['a] *)
type (-'a,+'b) binder

type ('a) variable
type 'a mvariable = 'a variable array
(** type of variables and array of variables *)

(** context, used to store names to rename free variables  *)
type context
val empty_context : context

(** this is the subtitution function: it takes an expression with a bound
  variable of type ['a] and a value for this variable and replace all the
  occurrences of this variable by this value *)
val subst : ('a,'b) binder -> 'a -> 'b

(** [binder_name f] returns the name of the variable bound in [f] *)
val binder_name : ('a,'b) binder -> string

(** [name_of v] returns the name of the free variable [v] *)
val name_of : 'a variable -> string

(** a safe comarison for variables *)
val compare_variables : 'a variable -> 'a variable -> int

val get_var_key : 'a variable -> int

(** type inhabited by data structures of type ['a] with variables under construction *)
type (+'a) bindbox

(** the function to call when the construction of an expression of type ['a] is
   finished. *)
val unbox : 'a bindbox -> 'a

(** the function to use a variable inside a data structure *)
val bindbox_of : 'a variable -> 'a bindbox

(** the function to use a variable as free, identical to the composition
    of [unbox] and [bindbox_of] *)
val free_of : 'a variable -> 'a

(** [unit] allows you to use an expression of type ['a] in a larger expression
   constructed with free variables *) 
val unit : 'a -> 'a bindbox

(** this is the function that allows you to construct expressions by allowing
  the application of a function with free variables to an argument with free
  variables *)
val apply : 
 ('a -> 'b) bindbox -> 'a bindbox -> 'b bindbox

(** It is sometimes usefull to have a dummy value, for instance to initialize arrays*)
val dummy_bindbox : 'a bindbox
(** If one use [dummy_bindbox] in a data structure, calling unbox will raise the exception
    [Failure "Invalid use of dummy_bindbox"] *)

(** check if the bound variable occurs or not *)
val is_binder_constant :
 ('a,'b) binder -> bool

(** check if a binder is a closed term *)
val is_binder_closed :
 ('a,'b) binder -> bool

(** {2 Multiple binders} *)

(** this is the type of an expression of type ['b] with several bound variables of 
 type 'a *)
type ('a,'b) mbinder

(** [mbinder_arity f] returns the number of variables bound in [f] *)
val mbinder_arity : ('a,'b) mbinder -> int
val binder_arity : ('a,'b) mbinder -> int

(** [mbinder_names f] returns the names of the variables bound in [f] *)
val mbinder_names : ('a,'b) mbinder -> string array
val binder_names : ('a,'b) mbinder -> string array

(** this is the subtitution function: it takes an expression with several bound
  variables of type ['a] and an array of values for these variables and replace all the
  occurrences by the given values *)
val msubst : ('a,'b) mbinder -> 'a array -> 'b

(** check if one of the bound variables occurs or not. There are no way to tell if a
    specific variable bound in a multiple binder occurs or not *)
val is_mbinder_constant :
 ('a,'b) mbinder -> bool

(** check is the term is a closed term *)
val is_mbinder_closed :
 ('a,'b) mbinder -> bool

(** {2 Other functions} *)

(** this function tells you if a ['a bindbox] is closed. This means it has no free variables. 
This may be useful when optimizing a program *)
val is_closed : 'a bindbox -> bool

val list_variables : 'a bindbox -> unit

val bind_apply : 
 ('a, 'b) binder bindbox -> 'a bindbox -> 'b bindbox
val mbind_apply : 
 ('a, 'b) mbinder bindbox -> 'a array bindbox -> 'b bindbox
(** These functions are usefull when using "higher order variables". That is variables that 
    represent itself binder and therefore that can be applied to arguments *)

(** This function and the following ones can be written using [unit] and [apply] but are given
 because they are very often used. Moreover, some of them are optimised *)
val unit_apply : 
 ('a -> 'b) -> 'a bindbox -> 'b bindbox
(** [unit_apply f a = apply (unit f) a]*)

(** [unit_apply2 f a b = apply (apply (unit f) a) b]*)
val unit_apply2 : 
 ('a -> 'b -> 'c) -> 'a bindbox -> 'b bindbox -> 'c bindbox

(** [unit_apply3 f a b c = apply (apply (apply (unit f) a) b) c]*)
val unit_apply3 : 
 ('a -> 'b -> 'c -> 'd) -> 'a bindbox -> 
   'b bindbox  -> 'c bindbox -> 'd bindbox

(** [lift_pair (x,y) = unit_apply2 (,) x y]*)
val lift_pair :
 'a bindbox -> 'b bindbox -> ('a * 'b) bindbox

(** Very advanced feature: binder fixpoint !*)
val fixpoint : 
    (('a, 'b) binder, ('a, 'b) binder) binder bindbox -> ('a, 'b) binder bindbox

(** The following structures allow you to define function like
  [lift_list] or [lift_array] for your own types, if you can provide a [map]
  function for your types. These are given for polymorphic types with
  one or two parameters *)

module type Map = 
  sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

module Lift(M: Map) :   
  sig
    val f : 'a bindbox M.t -> 'a M.t bindbox
  end


module type Map2 = 
  sig
    type ('a, 'b) t
    val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
  end

module Lift2(M: Map2) : 
  sig
    val f : ('a bindbox, 'b bindbox) M.t -> ('a, 'b) M.t bindbox
  end

(** {[lift_list = 
  let module M = struct 
    type 'a t = 'a list 
    let map = List.map end 
  in Lift(M).f]}*)
val lift_list :
 'a bindbox list -> 'a list bindbox

(** {[lift_array = 
  let module M = struct 
    type 'a t = 'a array 
    let map = Array.map end 
  in Lift(M).f]}*)
val lift_array :
 'a bindbox array -> 'a array bindbox

(**/**)
(* the camlp4 syntax extension should be prefered to the direct use of these functions *)

val bind :
 ('a variable -> 'a) -> string -> ('a bindbox -> 'b bindbox) ->
   ('a,'b) binder bindbox

val bind_in : context ->
 ('a variable -> 'a) -> string -> ('a bindbox -> context -> 'b bindbox) ->
   ('a,'b) binder bindbox

val reset_bindlib_count : unit -> unit 
val new_var : ('a variable -> 'a) -> string -> 'a variable

val new_var_in : context -> ('a variable -> 'a) -> string -> 'a variable * context

val bind_var : 'a variable -> 'b bindbox -> ('a, 'b) binder bindbox

val mbind :
 ('a variable -> 'a) -> string array -> 
   ('a bindbox array -> 'b bindbox) ->
   ('a,'b) mbinder bindbox

val mbind_in : context ->
 ('a variable -> 'a) -> string array -> 
   ('a bindbox array -> context -> 'b bindbox) ->
   ('a,'b) mbinder bindbox

val new_mvar : ('a variable -> 'a) -> string array -> 'a mvariable
val new_mvar_in : 
    context -> ('a variable -> 'a) -> string array -> 'a mvariable * context

val bind_mvar : 'a mvariable -> 'b bindbox -> ('a, 'b) mbinder bindbox

(* Not to be used by the casual user, only provided for the 
   camlp4 extension *)

type environment
type varpos
type 'a env_term = varpos -> environment -> 'a

val special_apply : unit bindbox -> 'a bindbox -> unit bindbox * 'a env_term

val special_start : unit bindbox
val special_end : unit bindbox -> 'a env_term -> 'a bindbox

(* This function creates a copy the the given variable, that is not
   distinguishable from the original variable when it is bound. However,
   when it is free (that is not bound when calling unbox), the way 
   to make it free may be different. This can be used to mark some specific
   occurence of a variable, the marking disappearing when the variable is free.   The name can also be changed *)
val copy_var : 'a variable -> string -> ('a variable -> 'a) -> 'a variable
