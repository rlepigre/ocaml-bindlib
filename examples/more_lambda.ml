open Bindlib

(* Type of the terms of the λ-calculus. *)
type term =
  | Var of term var
  | Abs of (term, term) binder
  | App of term * term

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

(* Weak head normalisation. *)

let rec whnf : term -> term = fun t ->
  match t with
  | App(t1,t2) ->
      begin
        match whnf t1 with
        | Abs(b) -> whnf (Bindlib.subst b t2)
        | u      -> if u == t1 then t (* optimisation *) else App(u, t2)
      end
  | t          -> t

(* Call-by-name normalisation. *)

let norm : term -> term = fun t ->
  let rec norm t =
    match whnf t with
    | Abs(b) -> let (x, t) = Bindlib.unbind b in abs x (norm t)
    | t      ->
    let rec unwind t =
      match t with
      | Var(x)     -> var x
      | App(t1,t2) -> app (unwind t1) (norm t2)
      | _          -> assert false (* unreachable *)
    in
    unwind t
  in
  Bindlib.unbox (norm t)

(* Example of complex variable manipulation: application marking. Here, [mark]
   is a function that can be described as follows using mathematical notations
   (with implicit variable renaming, i.e., assuming x does not occur in t):
   - mark(t) = λx.Φ(x,t)
   - Φ(x,y   ) = y
   - Φ(x,λy.t) = λy.Φ(x,t)
   - Φ(x,t u ) = Φ(x,t) Φ(x,u) *)

let mark t =
  let rec phi x t =
    match t with
    | Var(y)   -> var y
    | App(t,u) -> app (app (var x) (phi x t)) (phi x u)
    | Abs(b)   -> let (y, t) = Bindlib.unbind b in abs y (phi x t)
  in
  let x = Bindlib.new_var mkfree "x" in
  unbox (abs x (phi x t))
