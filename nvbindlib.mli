(**
Bindlib library
@author Christophe Raffalli
*)

(** This libraries provide functions and type to use binder (that is
construct that binds a variable in a data structure) *)

(** this is the type of an expression of type 'b with a bound variables of 
 type 'a *)
type ('a,'b) binder = 'a -> 'b

type 'a variable
type 'a var = 'a variable
type 'a mvariable = 'a variable array

(** this is the subtitution function: it takes an expression with a bound
  variable of type 'a and a value for this variable and replace all the
  occurrences of this variable by this value *)
val subst : ('a, 'b) binder -> 'a -> 'b

(** the type of an object of type 'a being constructed : this object may have
   free variables *)
type 'a bindbox

(** the function to call when the construction of an expression of type 'a is
   finished. 
   This two functions are identical, and can also be used as a prefixoperator 
   !!
*)
val bindbox_of : 'a variable -> 'a bindbox
val free_of : 'a variable -> 'a
val unbox : 'a bindbox -> 'a
val unlift : 'a bindbox -> 'a
val ( !! ) : 'a bindbox -> 'a

(** unit allows you to use an expression of type 'a in a larger expression
   begin constructed with free variables *) 
val unit : 'a -> 'a bindbox
val lift : 'a -> 'a bindbox
val ( !^ ) : 'a -> 'a bindbox

(** this is THE function constructing binder. If takes an expression of type
 'a bindbox -> 'b bindbox in general written (fun x -> expr) (we say that
  x is a free variable of expr). And it constructs the expression where x is 
  bound.
  The first argument is a function build a free variables.
  The second argument is the name of the variable
 *)
val bind :
 ('a variable -> 'a) -> ('a bindbox -> 'b bindbox) ->
   ('a,'b) binder bindbox

val new_var : ('a variable -> 'a) -> 'a variable

val bind_var : 'a variable -> 'b bindbox -> ('a, 'b) binder bindbox

(** this is THE function that allows you to construct expressions by allowing
  the application of a function with free variables to an argument with free
  variables *)
val apply : 
 ('a -> 'b) bindbox -> 'a bindbox -> 'b bindbox

val dummy_bindbox : 'a bindbox

(** this is the type of an expression of type 'b with "n" bound variables of 
 type 'a *)
type ('a,'b) mbinder

(** mbinder_arity f return the number of variables bound by *)
val mbinder_arity : ('a,'b) mbinder -> int

(** this is the subtitution function: it takes an expression with a bound
  variable of type 'a and a value for this variable and replace all the
  occurrences of this variable by this value *)
val msubst : ('a,'b) mbinder -> 'a array -> 'b

(** this is THE function constructing mbinder. If takes an expression of type
 'a bindbox array -> 'b bindbox in general written (fun x -> expr) 
   (we say that x is vector of free variables of expr). 
   And it constructs the expression where x's are 
  bound.
  The first argument is a function to build a free variables.
  The second argument are the names of the variable
 *)

val mbind :
 ('a variable -> 'a) -> int -> 
   ('a bindbox array -> 'b bindbox) ->
   ('a,'b) mbinder bindbox

val new_mvar : ('a variable -> 'a) -> int -> 'a mvariable

val bind_mvar : 'a mvariable -> 'b bindbox -> ('a, 'b) mbinder bindbox

(** check if the bound variable occurs or not *)
val is_binder_constant :
 ('a,'b) binder -> bool

(** check if a binder is a closed term *)
val is_binder_closed :
 ('a,'b) binder -> bool

(** check if at least one of the bound variables occurs *)
val is_mbinder_constant :
 ('a,'b) mbinder -> bool

(** check if a binder is a closed term *)
val is_mbinder_closed :
 ('a,'b) mbinder -> bool

(** Used in some rare cases ! *)
val bind_apply : 
 ('a, 'b) binder bindbox -> 'a bindbox -> 'b bindbox

val mbind_apply : 
 ('a, 'b) mbinder bindbox -> 'a array bindbox -> 'b bindbox

val fixpoint : 
    (('a, 'b) binder, ('a, 'b) binder) binder bindbox -> ('a, 'b) binder bindbox

(** The following function can be written using unit and apply but are given
 because they are very usefull. Moreover, some of them are optimised *)
val unit_apply : 
 ('a -> 'b) -> 'a bindbox -> 'b bindbox

val unit_apply2 : 
 ('a -> 'b -> 'c) -> 'a bindbox -> 'b bindbox -> 'c bindbox

val unit_apply3 : 
 ('a -> 'b -> 'c -> 'd) -> 'a bindbox -> 
   'b bindbox  -> 'c bindbox -> 'd bindbox

val lift_pair :
 'a bindbox -> 'b bindbox -> ('a * 'b) bindbox

val lift_list :
 'a bindbox list -> 'a list bindbox
val ( ^:: ) :
 'a bindbox -> 'a list bindbox -> 'a list bindbox

val lift_array :
 'a bindbox array -> 'a array bindbox

(** this function tells you if a 'a bindbox is closed. This may be
useful when optimizing a program *)
val is_closed : 'a bindbox -> bool

(** the following structures allow you to define function like
  lift_pair or lift_list for your own types, if you can provide a "map"
  function for your types. These are given for polymorphic types with
  one or two arguments *)

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

(* Not to be used by the casual user, only provided for the 
   camlp4 extension *)

type environment
type varpos
type 'a env_term = varpos -> environment -> 'a

val special_apply : unit bindbox -> 'a bindbox -> unit bindbox * 'a env_term

val special_start : unit bindbox
val special_end : unit bindbox -> 'a env_term -> 'a bindbox



