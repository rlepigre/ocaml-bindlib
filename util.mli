module Env : sig
  type any = Obj.t
  type environment =
    { tab          : any array
    ; mutable next : int }

  val create_env : int -> environment
  val set_env : environment -> int -> 'a -> unit
  val get_env : environment -> int -> 'a
  val dup_env : environment -> environment
end

module IMap : sig
  type (+'a) t
  type key = int
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : int -> 'a -> 'a t -> 'a t
  val find : int -> 'a t -> 'a
  val remove : int -> 'a t -> 'a t
  val mem :  int -> 'a t -> bool
  val iter : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module SMap : sig
  type (+'a) t
  type key = string
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end
