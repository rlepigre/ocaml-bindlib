(* This file contains the code quoted in "bindlib.mli" as a trailing  example.
   It illustrates a very basic usage of [Bindlib] to represent and  manipulate
   the simple abstract syntax tree of the pure λ-calculus. *)

open Bindlib

(* Type of the terms of the λ-calculus. *)
type term =
  | Var of term var
  | Abs of (term, term) binder
  | App of term * term

(* Call-by-name evaluation function. *)
let rec eval : term -> term = fun t ->
  match t with
  | App(f,a) ->
      begin
        match eval f with
        | Abs(b) -> eval (subst b a)
        | _      -> t
      end
  | _        -> t

(* The common syntactic wrapper for variables. *)
let mkfree : term var -> term = fun x -> Var(x)

(* Conversion of a λ-term into a [string]. *)
let to_string : term -> string = fun t ->
  let rec to_string ctxt t =
    match t with
    | Var(x)   -> name_of x
    | Abs(b)   -> let (x,t,ctxt) = unbind_in ctxt b in
                  "λ" ^ name_of x ^ "." ^ to_string ctxt t
    | App(t,u) -> "(" ^ to_string ctxt t ^ ") " ^ to_string ctxt u
  in
  to_string empty_ctxt t

(* Smart constructors. *)
let var : term var -> term box =
  fun x -> box_var x

let abs : term var -> term box -> term box =
  fun x t -> box_apply (fun b -> Abs(b)) (bind_var x t)

let app : term box -> term box -> term box =
  fun t u -> box_apply2 (fun t u -> App(t,u)) t u

(* NOTE the [var] smart constructor is only used with free variables. They can
   be created using [new_var]. *)

(* Examples of terms. *)
let id: term = (* \x.x *)
  let x = new_var mkfree "x" in
  unbox (abs x (var x))

let fst : term = (* \x.\y.x *)
  let x = new_var mkfree "x" in
  let y = new_var mkfree "y" in
  unbox (abs x (abs y (var x)))

let delta : term box = (* \x.(x) x (boxed) *)
  let x = new_var mkfree "x" in
  abs x (app (var x) (var x))

let omega : term = (* (\x.(x) x) \x.(x) x *)
  unbox (app delta delta)

let delta : term = (* \x.(x) x *)
  unbox delta

(* Tests. *)
let _ =
  Printf.printf "%s\n%!" (to_string id);
  Printf.printf "%s\n%!" (to_string fst);
  Printf.printf "%s\n%!" (to_string delta);
  Printf.printf "%s\n%!" (to_string omega)
