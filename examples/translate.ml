open Bindlib

(** this example illustrates the use of [copy_var] to translate from
    one AST to another without environment to associate variables of
    the first AST to variables of second AST

    It also illustrated the difference between vbind and bind_var.
*)

type term1 =
  | Var1 of term1 var
  | App1 of term1 * term1
  | Lam1 of (term1, term1) binder

let mkvar1 x = Var1 x
let safe_mkvar1 x = assert false
let app1 = box_apply2 (fun t u -> App1(t,u))
let lam1 name f = box_apply (fun f -> Lam1 f) (vbind safe_mkvar1 name f)

type term2 =
  | Var2 of term2 var
  | App2 of term2 * term2
  | Lam2 of (term2, term2) binder

let mkvar2 x = Var2 x
let safe_mkvar2 x = assert false
let app2 = box_apply2 (fun t u -> App2(t,u))
let lam2 name f = box_apply (fun f -> Lam2 f) (vbind safe_mkvar2 name f)
let lam2' x t = box_apply (fun f -> Lam2 f) (bind_var x t)

let var1_to_var2 x = copy_var x (name_of x) safe_mkvar2
let var2_to_var1 x = copy_var x (name_of x) safe_mkvar1

(* translation using bind_var *)
let translate1 t =
  let rec tr : term1 -> term2 bindbox = function
    | Var1 x -> box_of_var (var1_to_var2 x)
    | App1(t,u) -> app2 (tr t) (tr u)
    | Lam1(f) ->
       let (x,t) = unbind mkvar1 f in
       lam2' (var1_to_var2 x) (tr t)
  in unbox (tr t)

(* translation using vbind *)
let translate2 t =
  let rec tr : term1 -> term2 bindbox = function
    | Var1 x -> box_of_var (var1_to_var2 x)
    | App1(t,u) -> app2 (tr t) (tr u)
    | Lam1(f) ->
       lam2 (binder_name f) (fun x ->
         tr (subst f (Var1 (var2_to_var1 x))))
  in unbox (tr t)

(* translation using bind is not possible *)

let rec print2 ch = function
  | Var2 x -> Printf.fprintf ch "%s" (name_of x)
  | App2(t,u) -> Printf.fprintf ch "(%a)%a" print2 t print2 u
  | Lam2 f ->
     let (x,t) = unbind mkvar2 f in
     Printf.fprintf ch "\\%s.%a" (name_of x) print2 t

let idt1 = unbox (lam1 "x" (fun x -> box_of_var x))
let idt2 = translate1 idt1
let idt2' = translate2 idt1
let _ = Printf.printf "%a %a\n%!" print2 idt2 print2 idt2'

let is_idt2 t = match t with
  | Lam2 f -> let (x,t) = unbind mkvar2 f in
              let t = subst f (Var2 x) in
              (match t with
              | Var2 y -> eq_vars x y
              | _ -> false)
  | _ -> false

let _ = assert (is_idt2 idt2)
let _ = assert (is_idt2 idt2')
