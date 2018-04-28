(* This example illustrates the use of [Bindlib] to represent and manipulate a
   simple abstract syntax tree corresponding to the pure λ-calculus. *)

open Bindlib

(* AST of the pure λ-calculus using free variables and binders. *)
type term =
  | Var of term var
  | Abs of (term, term) binder
  | App of term * term

(* The often required [mkfree] function. *)
let mkfree : term var -> term =
  fun x -> Var(x)

(* Smart constructors to build terms in the [box]. *)
let var : term var -> term box =
  box_of_var

let abs : term var -> term box -> term box =
  fun x t -> box_apply (fun b -> Abs(b)) (bind_var x t)

let app : term box -> term box -> term box =
  fun t u -> box_apply2 (fun t u -> App(t,u)) t u

(* A function to create a fresh variable. *)
let fresh_var : string -> term var =
  fun x -> new_var mkfree x

(* NOTE two calls to [fresh_var] will produce two distinct variables. *)

(* Some examples of terms (not unboxed). *)
let x     : term var = fresh_var "x"
let y     : term var = fresh_var "y"
let id    : term box = abs x (var x)
let fst   : term box = abs x (abs y (var x))
let delta : term box = abs x (app (var x) (var x))
let omega : term box = app delta delta

(* An example of term constructed using [var] and [abs_var]. *)
let swap : term box =
  let x = fresh_var "x" in
  let y = fresh_var "y" in
  let t = app (var y) (var x) in
  abs x (abs y t)

(* Examples producing capture during evaluation. *)
let fst_y  : term box = app fst (var y)
let fst_yx : term box = app fst_y (var x)
let swap_y : term box = app swap (var y)

(* Unboxed terms. *)
let x      : term = unbox (var x)
let y      : term = unbox (var y)
let id     : term = unbox id
let fst    : term = unbox fst
let delta  : term = unbox delta
let omega  : term = unbox omega
let swap   : term = unbox swap
let fst_y  : term = unbox fst_y
let fst_yx : term = unbox fst_yx
let swap_y : term = unbox swap_y

(* Translation to string. *)
let rec to_string : term -> string = fun t ->
  match t with
  | Var(x)   -> name_of x
  | Abs(b)   -> let (x,t) = unbind b in
                "λ" ^ name_of x ^ "." ^ to_string t
  | App(t,u) -> "(" ^ to_string t ^ ") " ^ to_string u

(* Printing function. *)
let rec print : out_channel -> term -> unit = fun ch t ->
  match t with
  | Var(x)   -> Printf.fprintf ch "%s" (name_of x)
  | Abs(b)   -> let (x,t) = unbind b in
                Printf.fprintf ch "λ%s.%a" (name_of x) print t
  | App(t,u) -> Printf.fprintf ch "(%a) %a" print t print u

(* Lifting to the [box]. *)
let rec lift : term -> term box = fun t ->
  match t with
  | Var(x)   -> box_of_var x
  | Abs(b)   -> let (x,t) = unbind b in
                abs x (lift (subst b (Var(x))))
  | App(u,v) -> app (lift u) (lift v)

(* Update function to recompute names (required after substitution). *)
let update : term -> term = fun t ->
  unbox (lift t)

(* One step evaluation function (call-by-name). *)
let rec step : term -> term option = fun t ->
  match t with
  | App(Abs(b), u) -> Some (subst b u)
  | App(t     , u) ->
      begin
        match step t with
        | None   -> None
        | Some t -> Some (App(t,u))
      end
  | _              -> None

(* Full evaluation (call-by-name). *)
let rec eval : term -> term = fun t ->
  match step t with
  | None   -> t
  | Some t -> eval t

(* Tests. *)
let _ =
  Printf.printf "Here are some terms:\n%!";
  Printf.printf "  %a\n%!" print x;
  Printf.printf "  %a\n%!" print y;
  Printf.printf "  %a\n%!" print id;
  Printf.printf "  %a\n%!" print fst;
  Printf.printf "  %a\n%!" print delta;
  Printf.printf "  %a\n%!" print omega;
  Printf.printf "  %a\n%!" print swap;
  Printf.printf "  %a\n%!" print fst_y;
  Printf.printf "  %a\n%!" print fst_yx;
  Printf.printf "  %a\n%!" print swap_y

let _ =
  Printf.printf "Substitution is fast, but does not handle renaming:\n%!";
  Printf.printf "  %a\n\t→ %a\n%!" print fst_y  print (eval fst_y);
  Printf.printf "  %a\n\t→ %a\n%!" print fst_yx print (eval fst_yx);
  Printf.printf "  %a\n\t→ %a\n%!" print swap_y print (eval swap_y)

let _ =
  Printf.printf "For printing, it is better to update names first:\n%!";
  Printf.printf "  %a\n\t→ %a\n%!" print fst_y  print (update (eval fst_y));
  Printf.printf "  %a\n\t→ %a\n%!" print fst_yx print (update (eval fst_yx));
  Printf.printf "  %a\n\t→ %a\n%!" print swap_y print (update (eval swap_y))
