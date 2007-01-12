open Bindlib

module StringCmp =
  struct
    type t = string
    let compare = compare
  end
    
module StringSet = Set.Make(StringCmp)
module StringMap = Map.Make(StringCmp)

exception Quit

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

| UVar of (term, term) mbinder array * int * term array
| Var of term var
| Varint of int
| Dummy


and pattern =
  int array * 
      ((term, term) mbinder, term) mbinder *
      ((term, term) mbinder, term) mbinder

let var x = Var(x)

let lvar l = assert false

let bnth i tl =
  unit_apply (fun l -> List.nth l i) tl

let bproj i ts =
  unit_apply (fun t -> t.(i)) ts

let bmeta i ts tl =
  bind_apply (bproj i ts) (lift_list tl)

