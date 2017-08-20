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

(* Smart constructors to build terms in the [bindbox]. *)
let abs : string -> (term bindbox -> term bindbox) -> term bindbox =
  fun x f -> box_apply (fun b -> Abs(b)) (bind mkfree x f)

let app : term bindbox -> term bindbox -> term bindbox =
  fun t u -> box_apply2 (fun t u -> App(t,u)) t u

(* Alternative smart constructor for [Abs], using [bind_var]. *)
let abs_var : term var -> term bindbox -> term bindbox =
  fun v t -> box_apply (fun b -> Abs(b)) (bind_var v t)

(* A function to create a fresh variable. *)
let var : string -> term var =
  fun x -> new_var mkfree x

(* NOTE two calls to [var] will produce two distinct variables. *)

(* Some examples of terms (not unboxed). *)
let x     : term bindbox = box_of_var (var "x")
let y     : term bindbox = box_of_var (var "y")
let id    : term bindbox = abs "x" (fun x -> x)
let fst   : term bindbox = abs "x" (fun x -> abs "y" (fun y -> x))
let delta : term bindbox = abs "x" (fun x -> app x x)
let omega : term bindbox = app delta delta

(* An example of term constructed using [var] and [abs_var]. *)
let swap : term bindbox =
  let x = var "x" in
  let y = var "y" in
  let t = app (box_of_var y) (box_of_var x) in
  abs_var x (abs_var y t)

(* Examples producing capture during evaluation. *)
let fst_y  : term bindbox = app fst y
let fst_yx : term bindbox = app fst_y x
let swap_y : term bindbox = app swap y

(* Unboxed terms. *)
let x      : term = unbox x
let y      : term = unbox y
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
  | Abs(b)   -> let (x,t) = unbind mkfree b in
                "λ" ^ name_of x ^ "." ^ to_string t
  | App(t,u) -> "(" ^ to_string t ^ ") " ^ to_string u

(* Printing function. *)
let rec print : out_channel -> term -> unit = fun ch t ->
  match t with
  | Var(x)   -> Printf.fprintf ch "%s" (name_of x)
  | Abs(b)   -> let (x,t) = unbind mkfree b in
                Printf.fprintf ch "λ%s.%a" (name_of x) print t
  | App(t,u) -> Printf.fprintf ch "(%a) %a" print t print u

(* Lifting to the [bindbox]. *)
let rec lift : term -> term bindbox = fun t ->
  match t with
  | Var(x)   -> box_of_var x
  | Abs(b)   -> let (x,t) = unbind mkfree b in
                abs_var x (lift (subst b (Var(x))))
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
