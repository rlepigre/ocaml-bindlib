open Bindlib

module StringCmp =
  struct
    type t = string
    let compare = compare
  end

module StringSet = Set.Make(StringCmp)
module StringMap = Map.Make(StringCmp)

exception Unbound of string

type assoc =
    Left | Right | NonAssoc

type fix =
    Prefix | Infix of assoc

type symbol =
  {
    symbol_arity : int;
    symbol_name : string;
    mutable symbol_rewrite : pattern list;
    symbol_fix : fix;
    symbol_priority : float
  }

and term =
  Bind of bool * string * (term, term) binder
| App of bool * bool * symbol * term array
| Def of string * bool ref * term

| UVar of int * term array
| PVar of string * term array
| Var of term var
| Varint of int
| Dummy

and pattern =
  int array * term * ((term, term) mbinder, term) mbinder

let var x = Var(x)

let lvar l = assert false

let app b2 s a =
  let a = box_array a in
  let b1 = is_closed a in
  box_apply (fun a -> App(b1,b2,s,a)) a

let tbind s f =
  let f = bind var s f in
  let b = is_closed f in
  box_apply (fun f -> Bind(b,s,f)) f

let uvar x a =
  box_apply (fun a -> UVar(x,a)) (box_array a)

let pvar x a =
  box_apply (fun a -> PVar(x,a)) (box_array a)

let bnth i tl =
  box_apply (fun l -> l.(i)) (box_array tl)

let bproj i ts =
  box_apply (fun t -> t.(i)) ts

let bmeta i ts tl =
  mbind_apply (bproj i ts) (box_array tl)

let bind_reduction t u =
  let l = ref [] in
  let rec fn = function
    | Bind(_,s,f) -> fn (subst f Dummy)
    | App(_,n,s,a) -> Array.iter fn a
    | PVar(name,a) ->
       if List.mem_assoc name !l then failwith "Non linear pattern";
       l := (name, Array.length a) :: !l;
       Array.iter fn a
    | _ -> ()
  in
  fn t;
  let args = Array.of_list !l in
  let index name =
    let res = ref (0,0) in
    for i = 0 to Array.length args - 1 do
      if fst args.(i) = name then res := (i,snd args.(i))
    done;
    !res
  in
  let ma = new_mvar (fun _ -> assert false) (Array.map fst args) in
  let rec fn = function
    | Bind(_,s,f) ->
       tbind s (fun x -> fn (subst f (unbox x)))
    | App(_,n,s,a) ->
       app n s (Array.map fn a)
    | Var v -> box_of_var v
    | PVar(name,a) ->
       let i, _ = index name in
       uvar i (Array.map fn a)
    | t -> box t
  in
  let rec gn = function
    | Bind(_,s,f) ->
       tbind s (fun x -> gn (subst f (unbox x)))
    | App(_,n,s,a) ->
       app n s (Array.map gn a)
    | Var v -> box_of_var v
    | PVar(name,a) ->
       let i,arity = index name in
       if Array.length a <> arity then failwith "Arity mismatch";
       bmeta i (box_array (Array.map box_of_var ma)) (Array.map gn a)
    | t -> box t
  in
  Array.map snd args, unbox (fn t), unbox (bind_mvar ma (gn u))
