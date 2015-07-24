(* An environment is used to store the value of every bound variables. We
   need to use Obj since bound variables may have different types and we
   want to store them in a single array. *)
module Env = struct
  type any = Obj.t 
  type environment =
    { tab          : any array (* An array with elements of any type. *)
    ; mutable next : int }     (* Next free cell of the array. *)
  
  (* Creates an empty environment of a given size. *)
  let create_env : int -> environment =
    fun size ->
      let dummy = Obj.repr () in
      { tab = Array.make size dummy; next = 0 }
  
  (* Sets the value stored at some position in the environment. *)
  let set_env : environment -> int -> 'a -> unit =
    fun env i e -> Array.set env.tab i (Obj.repr e)
  
  (* Gets the value stored at some position in the environment. *)
  let get_env : environment -> int -> 'a =
    fun env i -> Obj.obj (Array.get env.tab i)
  
  (* Make a copy of the environment. *)
  let dup_env : environment -> environment =
    fun env -> { tab = Array.copy env.tab; next = env.next }
end

module IMap = Ptmap

module SMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)
