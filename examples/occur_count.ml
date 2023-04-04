(* This example shows how to keep information about the free variables of a
   term in a custom "box" type. *)

open Bindlib

(* AST of the pure λ-calculus using free variables and binders. *)
type term =
  | Var of term var
  | Abs of (term, term) binder
  | App of term * term

module VarMap = Map.Make(struct
  type t = term var
  let compare = compare_vars
end)

type tvar = term var

type tbox = {
  (* Boxed term. *)
  t : term box;
  (* Mapping variables to number of occurences. *)
  m : int VarMap.t
}

let free_vars : tbox -> int VarMap.t = fun t -> t.m

let unbox t = unbox t.t

(* The often required [mkfree] function. *)
let mkfree : tvar -> term =
  fun x -> Var(x)

(* Smart constructors to build terms in the [box]. *)
let var : tvar -> tbox = fun v ->
  {t = box_var v; m = VarMap.singleton v 1}

let box : term -> tbox = fun t ->
  {t = box t; m = VarMap.empty}

let abs : tvar -> tbox -> tbox = fun x t ->
  let m = VarMap.remove x t.m in
  let t = box_apply (fun b -> Abs(b)) (bind_var x t.t) in
  {t; m}

let app : tbox -> tbox -> tbox = fun t u ->
  let merge _ n1 n2 = Some(n1 + n2) in
  let m = VarMap.union merge t.m u.m in
  let t = box_apply2 (fun t u -> App(t,u)) t.t u.t in
  {t; m}

let fresh_var : string -> tvar =
  fun x -> new_var mkfree x

let x : tvar = fresh_var "x"
let y : tvar = fresh_var "y"
let t : tbox = app (var x) (app (app (var y) (var x)) (var x))

let _ =
  let pp_mapping v n =
    Printf.printf "There are %i occurrences of variable %s.\n%!" n (name_of v)
  in
  VarMap.iter pp_mapping (free_vars t)
