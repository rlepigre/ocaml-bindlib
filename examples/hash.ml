type tm =
  | It
  | Var of tm Bindlib.var
  | Lam of ty * (tm, tm) Bindlib.binder
  | App of tm * tm
  | Pair of tm * tm
  | ProjL of tm
  | ProjR of tm
  | VHash of int (* Only used for hashing. *)

let hash : tm -> int = fun t ->
  let next : unit -> int =
    let counter = ref (-1) in
    fun _ -> incr counter; VHash(!counter)
  in
  let rec hash t =
    match t with
    | It        -> Hashtbl.hash (`It)
    | Var(x)    -> Hashtbl.hash (`Var  , Bindlib.hash_var x) (* Free var. *)
    | Lam(_,b)  -> Hashtbl.hash (`Lam  , hash (Bindlib.subst b (next ())))
    | App(t,u)  -> Hashtbl.hash (`App  , hash t, hash u)
    | Pair(t,u) -> Hashtbl.hash (`Pair , hash t, hash u)
    | ProjL(t)  -> Hashtbl.hash (`ProjL, hash t)
    | ProjR(t)  -> Hashtbl.hash (`ProjR, hash t)
    | VHash(i)  -> i
  in
  hash t
