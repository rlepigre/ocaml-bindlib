(* This example illustrates the way one can convert an AST without binders (or
   the output of a parser) into an AST built using [Bindlib]. *)

open Bindlib

(* AST of the pure λ-calculus. *)
type term =
  | Var of term var
  | Abs of (term, term) binder
  | App of term * term

(* Utilities and smart constructors. *)
let mkfree : term var -> term =
  fun x -> Var(x)

let var : term var -> term bindbox =
  fun x -> box_var x

let abs : term var -> term bindbox -> term bindbox =
  fun x t -> box_apply (fun b -> Abs(b)) (bind_var x t)

let app : term bindbox -> term bindbox -> term bindbox =
  fun t u -> box_apply2 (fun t u -> App(t,u)) t u

(* Printing function. *)
let rec print : out_channel -> term -> unit = fun ch t ->
  match t with
  | Var(x)   -> Printf.fprintf ch "%s" (name_of x)
  | Abs(b)   -> let (x,t) = unbind b in
                Printf.fprintf ch "λ%s.%a" (name_of x) print t
  | App(t,u) -> Printf.fprintf ch "(%a) %a" print t print u

(* Example of parsing time AST for our language. *)
type pterm =
  | PVar of string
  | PLam of string * pterm
  | PApp of pterm * pterm

(* Translation function to our AST using bindlib. *)
let trans : pterm -> term =
  let rec trans : (string * term var) list -> pterm -> term bindbox =
    fun env t ->
      match t with
      | PVar(x)   -> var (List.assoc x env) (* unbound if not found *)
      | PLam(x,t) -> let v = new_var mkfree x in
                     abs v (trans ((x,v)::env) t)
      | PApp(t,u) -> app (trans env t) (trans env u)
  in
  fun t -> unbox (trans [] t)

(* Translation test. *)
let _ =
  let fst = PLam("x",PLam("y",PVar("x"))) in
  Printf.printf "Translated to [%a]!\n%!" print (trans fst)
