type any = Obj.t

(* An environment is used to store the value of every bound variables. We
   need to use Obj since bound variables may have different types and we
   want to store them in a single array. *)
module Env = struct
  type t =
    { tab          : any array (* An array with elements of any type. *)
    ; mutable next : int }     (* Next free cell of the array. *)

  (* Creates an empty environment of a given size. *)
  let create : int -> t =
    fun size ->
      let dummy = Obj.repr () in
      { tab = Array.make size dummy; next = 0 }

  (* Sets the value stored at some position in the environment. *)
  let set : t -> int -> 'a -> unit =
    fun env i e -> Array.set env.tab i (Obj.repr e)

  (* Gets the value stored at some position in the environment. *)
  let get : t -> int -> 'a =
    fun env i -> Obj.obj (Array.get env.tab i)

  (* Make a copy of the environment. *)
  let dup : t -> t =
    fun env -> { tab = Array.copy env.tab; next = env.next }

  (* Get next free cell index. *)
  let next : t -> int =
    fun env -> env.next

  (* Set the next free cell index. *)
  let set_next : t -> int -> unit =
    fun env n -> env.next <- n
end

module IMap = Ptmap

module SMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

let new_counter =
  let c = ref 1 in
  fun () ->
    let fresh () = let n = !c in incr c; n in
    let reset () = c := 1 in
    (fresh, reset)

let swap f a b = f b a

let filter_map cond fn l =
  let rec aux acc = function
    | []   -> List.rev acc
    | x::l -> if cond x then aux (fn x::acc) l else aux acc l
  in aux [] l
