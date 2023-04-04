(* This example shows how to keep information of the free variables of a
   term using bindlib *)

open Bindlib

(* AST of the pure λ-calculus using free variables and binders. *)
type term =
  | Var of term var
  | Abs of (term, term) binder
  | App of term * term

module Var = struct
  type t = term var
  let compare = compare_vars
end
module VarMap = Map.Make(Var)

type tvar = term var
type tbox = { t : term box; m : int VarMap.t }

let free_vars : tbox -> int VarMap.t = fun t -> t.m

let unbox t = unbox t.t

(* The often required [mkfree] function. *)
let mkfree : tvar -> term =
  fun x -> Var(x)

(* Smart constructors to build terms in the [box]. *)
let var : tvar -> tbox =
  fun v ->
  let m = VarMap.singleton v 1 in
  let t = box_var v in
  {t; m}

let box : term -> tbox =
  fun t -> {t = box t; m = VarMap.empty}

let abs : tvar -> tbox -> tbox =
  fun x t ->
    let m = VarMap.remove x t.m in
    let t = box_apply (fun b -> Abs(b)) (bind_var x t.t) in
    {t; m}

let app : tbox -> tbox -> tbox =
  fun t u ->
  let merge _ n1 n2 = Some(n1 + n2) in
  let m = VarMap.union merge t.m u.m in
  let t = box_apply2 (fun t u -> App(t,u)) t.t u.t in
  {t; m}

(* A function to create a fresh variable. *)
let fresh_var : string -> tvar =
  fun x -> new_var mkfree x

(* NOTE two calls to [fresh_var] will produce two distinct variables. *)

(* Some examples of terms (not unboxed). *)
let x     : tvar = fresh_var "x"
let y     : tvar = fresh_var "y"
let id    : tbox = abs x (var x)
let fst   : tbox = abs x (abs y (var x))
let delta : tbox = abs x (app (var x) (var x))
let omega : tbox = app delta delta
let two   : tbox = abs y (abs x (app (var y) (app (var y) (var x))))
let four  : tbox = app two two

(* An example of term constructed using [var] and [abs_var]. *)
let swap : tbox =
  let x = fresh_var "x" in
  let y = fresh_var "y" in
  let t = app (var y) (var x) in
  abs x (abs y t)

(* Examples producing capture during evaluation. *)
let fst_y  : tbox = app fst (var y)
let fst_yx : tbox = app fst_y (var x)
let swap_y : tbox = app swap (var y)

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
let two    : term = unbox two
let four   : term = unbox four

(* Translation to string. *)
let string : term -> string = fun t ->
  let rec fn ctxt t =
    match t with
    | Var(x)   -> name_of x
    | Abs(b)   -> let (x,t,ctxt) = unbind_in ctxt b in
                  "λ" ^ name_of x ^ "." ^ fn ctxt t
    | App(t,u) -> "(" ^ fn ctxt t ^ ") " ^ fn ctxt u
  in
  fn empty_ctxt t

(* Printing function, using context to rename variables if needed. *)
let print : out_channel -> term -> unit = fun ch t ->
  let rec fn ctxt ch t =
    match t with
    | Var(x)   -> Printf.fprintf ch "%s" (name_of x)
    | Abs(b)   -> let (x,t,ctxt) = unbind_in ctxt b in
                  Printf.fprintf ch "λ%s.%a" (name_of x) (fn ctxt) t
    | App(t,u) -> Printf.fprintf ch "(%a) %a" (fn ctxt) t (fn ctxt) u
  in
  fn empty_ctxt ch t

(* Lifting to the [box]. We use an optimisation for closed binders *)
let rec lift : term -> tbox = fun t ->
  match t with
  | Var(x)   -> var x
  | Abs(b)   -> if binder_closed b then box t
                else let (x,t) = unbind b in
                     abs x (lift t)
  | App(u,v) -> app (lift u) (lift v)

(* One step evaluation function (call-by-name).
   We have to normalise under binder and therefore to use the type tbox.
   Notice the use of the lift function when we do not perform normalisation.
  *)
let rec step : term -> tbox option = fun t ->
  match t with
  | App(Abs(b), u) -> Some (lift (subst b u))
  | App(t     , u) ->
      begin
        match step t with
        | None   ->
           begin
             match step u with
             | None   -> None
             | Some u -> Some (app (lift t) u)
           end
        | Some t -> Some (app t (lift u))
      end
  | Abs(b)         ->
      begin
        let (x,t) = unbind b in
        match step t with
        | None   -> None
        | Some t -> Some(abs x t)
      end
  | _              -> None

(* unboxed version of the above *)
let step : term -> term option = fun t ->
  match step t with
  | None   -> None
  | Some t -> Some (unbox t)

(* Full evaluation (call-by-name). *)
let rec eval : term -> term = fun t ->
  match step t with
  | None   -> t
  | Some t -> eval t

(* Call by name with a stack completely avoid the use of lift
   and is much shorter *)
let seval : term -> term = fun t ->
  let rec fn t stack =
    match (t, stack) with
    | (App(t,u), _) -> fn t (u::stack)
    | (Abs(b), u::stack) -> fn (subst b u) stack
    | (Abs(b), [])       -> let (x,t) = unbind b in
                            abs x (fn t [])
    | (Var(x), stack)    -> List.fold_left app (var x)
                              (List.map (fun t -> fn t []) stack)
  in
  unbox (fn t [])

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
  Printf.printf "  %a\n%!" print swap_y;
  Printf.printf "  %a\n%!" print two;
  Printf.printf "  %a\n%!" print four

let _ =
  Printf.printf "Here are some sevaluation... Bindlib is fast.\n";
  Printf.printf "Renaming of variables is delayed until printing.\n";
  Printf.printf "It uses context and policies to allow fine control.\n";
  Printf.printf "  %a\n\t→ %a\n%!" print fst_y  print (seval fst_y);
  Printf.printf "  %a\n\t→ %a\n%!" print fst_yx print (seval fst_yx);
  Printf.printf "  %a\n\t→ %a\n%!" print swap_y print (seval swap_y);
  Printf.printf "  %a\n\t→ %a\n%!" print four print (seval four)

(** We can customize printing : *)
module Custom_Renaming = struct
  type ctxt = string list

  let empty_ctxt = []

  let rec new_name s ctxt =
    if List.mem s ctxt then new_name (s ^ "'") ctxt
    else (s, s::ctxt)

  let reserve_name s ctxt =
    if List.mem s ctxt then ctxt else s :: ctxt

  let reset_context_for_closed_terms = false
  let skip_constant_binders = false
  let constant_binder_name = Some "_"
end

module Custom_Ctxt = Ctxt(Custom_Renaming)

                       (* Printing function, using custom renaming. *)
let print2 : out_channel -> term -> unit = fun ch t ->
  let open Custom_Ctxt in
  let rec fn ctxt ch t =
    match t with
    | Var(x)   -> Printf.fprintf ch "%s" (name_of x)
    | Abs(b)   -> let (x,t,ctxt) = unbind_in ctxt b in
                  Printf.fprintf ch "λ%s.%a" (name_of x) (fn ctxt) t
    | App(t,u) -> Printf.fprintf ch "(%a) %a" (fn ctxt) t (fn ctxt) u
  in
  fn empty_ctxt ch t

(* Tests. *)
let _ =
  Printf.printf "Same as above with the custom renaming:\n%!";
  Printf.printf "  %a\n%!" print2 x;
  Printf.printf "  %a\n%!" print2 y;
  Printf.printf "  %a\n%!" print2 id;
  Printf.printf "  %a\n%!" print2 fst;
  Printf.printf "  %a\n%!" print2 delta;
  Printf.printf "  %a\n%!" print2 omega;
  Printf.printf "  %a\n%!" print2 swap;
  Printf.printf "  %a\n%!" print2 fst_y;
  Printf.printf "  %a\n%!" print2 fst_yx;
  Printf.printf "  %a\n%!" print2 swap_y;
  Printf.printf "  %a\n%!" print two;
  Printf.printf "  %a\n%!" print four;
  Printf.printf "  %a\n\t→ %a\n%!" print2 fst_y  print2 (seval fst_y);
  Printf.printf "  %a\n\t→ %a\n%!" print2 fst_yx print2 (seval fst_yx);
  Printf.printf "  %a\n\t→ %a\n%!" print2 swap_y print2 (seval swap_y);
  Printf.printf "  %a\n\t→ %a\n%!" print2 four print2 (seval four)
