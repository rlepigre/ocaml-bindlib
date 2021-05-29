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

(* Size function computing the number of λ-abstractions and applications. *)
let rec size : term -> int = fun t ->
  match t with
  | Var(_)   -> 0
  | Abs(b)   -> let (_,t) = unbind b in
                1 + size t
  | App(t,u) -> 1 + size t + size u

(* Smart constructors. *)

let var : term var -> term box =
  fun x -> box_var x

let abs_raw : (term, term) binder box -> term box =
  fun b -> box_apply (fun b -> Abs(b)) b

let abs : term var -> term box -> term box =
  fun x t -> abs_raw (bind_var x t)

let app : term box -> term box -> term box =
  fun t u -> box_apply2 (fun t u -> App(t,u)) t u

let rec box_term : term -> term box = fun t ->
  match t with
  | Var(x)   -> var x
  | Abs(b)   -> abs_raw (box_binder box_term b)
  | App(t,u) -> app (box_term t) (box_term u)

(* Conversion of a λ-term into a [string] using Krivine's notation (this means
   that the application of term [t] to term [u] is written [(t) u]). *)
let rec to_string : ctxt -> term -> string = fun ctxt t ->
  match t with
  | Var(x)   -> name_of x
  | Abs(b)   -> let (x,t,ctxt) = unbind_in ctxt b in
                "λ" ^ name_of x ^ "." ^ to_string ctxt t
  | App(t,u) -> "(" ^ to_string ctxt t ^ ") " ^ to_string ctxt u

let to_string : term -> string = fun t ->
  to_string (free_vars (box_term t)) t

(* Examples of terms. *)

(* λx.x *)
let id : term =
  let x = new_var mkfree "x" in
  unbox (abs x (var x))

(* λx.λy.x *)
let fst : term =
  let x = new_var mkfree "x" in
  let y = new_var mkfree "y" in
  unbox (abs x (abs y (var x)))

(* λx.(x) x (boxed) *)
let delta : term box =
  let x = new_var mkfree "x" in
  abs x (app (var x) (var x))

(* (λx.(x) x) (λx.(x) x) *)
let omega : term =
  unbox (app delta delta)

(* λx.(x) x *)
let delta : term =
  unbox delta

(* Tests. *)
let _ =
  Printf.printf "id     (size = %2i): %s\n%!" (size id) (to_string id);
  Printf.printf "fst    (size = %2i): %s\n%!" (size fst) (to_string fst);
  Printf.printf "delta  (size = %2i): %s\n%!" (size delta) (to_string delta);
  Printf.printf "omega  (size = %2i): %s\n%!" (size omega) (to_string omega)

(* Define a bigger term and test evaluation. *)
let _ =
  let x = new_var mkfree "x" in
  let y = new_var mkfree "y" in
  let delta = abs x (app (var x) (var x)) in
  let omega = app delta delta in
  let fst = abs x (abs y (var x)) in
  let id = abs x (var x) in
  let t = unbox (app (app fst (app id (app delta id))) omega) in
  let v = eval t in
  Printf.printf "t      (size = %2i): %s\n%!" (size t) (to_string t);
  Printf.printf "eval t (size = %2i): %s\n%!" (size v) (to_string v)

(* Test with an open term. *)
let _ =
  let x = new_var mkfree "x" in
  let y = new_var mkfree "y" in
  let fst = abs x (abs y (var x)) in
  let t = unbox (app fst (var y)) in
  let v = eval t in
  Printf.printf "t      (size = %2i): %s\n%!" (size t) (to_string t);
  Printf.printf "eval t (size = %2i): %s\n%!" (size v) (to_string v)
