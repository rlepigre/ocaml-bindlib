(* This file contains the code quoted in "bindlib.mli" as a trailing  example.
   It illustrates a very basic usage of [Bindlib] to represent and  manipulate
   the simple abstract syntax tree of the pure λ-calculus. *)

open Bindlib

(* Type of the terms of the λ-calculus. *)
type term =
  | Var of term var
  | Abs of (term, term) binder
  | App of term * term

(* Evaluation function. *)
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
let rec to_string : term -> string = fun t ->
  match t with
  | Var(x)   -> name_of x
  | Abs(b)   -> let (x,t) = unbind mkfree b in
                "λ" ^ name_of x ^ "." ^ to_string t
  | App(t,u) -> "(" ^ to_string t ^ ") " ^ to_string u

(* Smart constructors. *)
let var : term var -> term bindbox =
  fun x -> box_of_var x

let abs : string -> (term bindbox -> term bindbox) -> term bindbox =
  fun x f -> box_apply (fun b -> Abs(b)) (bind mkfree x f)

let app : term bindbox -> term bindbox -> term bindbox =
  fun t u -> box_apply2 (fun t u -> App(t,u)) t u

(* NOTE the [var] smart constructor is only used with free variables. They can
   be created using [new_var]. In fact, [var] is also useful when we consider
   another smart constructor for λ-abstraction (see below). *)

(* Examples of terms. *)
let id: term = (* \x.x *)
  unbox (abs "x" (fun x -> x))

let fst : term = (* \x.\y.x *)
  unbox (abs "x" (fun x -> abs "y" (fun _ -> x)))

let omega : term = (* (\x.(x) x) \x.(x) x *)
  let delta = abs "x" (fun x -> app x x) in
  unbox (app delta delta)

(* Alternative smart constructor for λ-abstraction. *)
let abs_var : string -> (term var -> term bindbox) -> term bindbox =
  fun x f -> box_apply (fun b -> Abs(b)) (vbind mkfree x f)

(* Example of term using this new smart constructor (and [var]). *)
let delta : term = (* \x.(x) x *)
  unbox (abs_var "x" (fun x -> app (var x) (var x)))

(* Tests. *)
let _ =
  Printf.printf "%s\n%!" (to_string id);
  Printf.printf "%s\n%!" (to_string fst);
  Printf.printf "%s\n%!" (to_string omega);
  Printf.printf "%s\n%!" (to_string delta)
