open Bindlib

(* Abstract Syntax Tree of the pure λ-calculus using Bindlib variables and
binders. *)
type term =
  | Var of term variable
  | Lam of (term, term) binder
  | App of term * term

(* Terms are built in the [bindbox] type, we define smart constructors for
convenience. *)
let var x   = box_of_var (new_var (fun x -> Var x) x)
let lam x f = box_apply (fun b -> Lam b) (bind (fun x -> Var x) x f)
let vlam x f = box_apply (fun b -> Lam b) (vbind (fun x -> Var x) x f)
let app t u = box_apply2 (fun t u -> App(t,u)) t u

(* Function to destruct a binder, fixing the first argument. *)
let unbind f = unbind (fun x -> Var x) f

(* Some example of standard terms. *)
let x     = var "x"
let y     = var "y"
let id    = lam "x" (fun x -> x)
let fst   = lam "x" (fun x -> lam "y" (fun y -> x))
let delta = lam "x" (fun x -> app x x)
let omega = app delta delta
let fsty  = app fst y
let fstyx = app fsty x

(* Translation to string. *)
let rec term_to_string = function
  | Var x    -> name_of x
  | Lam b    -> let (x,t) = unbind b in
                "λ" ^ (name_of x) ^ (term_to_string t)
  | App(t,u) -> (term_to_string t) ^ "(" ^ (term_to_string u) ^ ")"

(* lifting *)
let rec lift_term = function
  | Var(y) -> box_of_var y
  | App(u,v) -> app (lift_term u)  (lift_term v)
  | Lam(f) ->
      vlam (binder_name f) (fun x -> lift_term (subst f (Var x)))

(* Printing function. *)
let rec print_term ch = function
  | Var x    -> Printf.fprintf ch "%s" (name_of x)
  | Lam b    -> let (x,t) = unbind b in
                Printf.fprintf ch "λ%s.%a" (name_of x) print_term t
  | App(t,u) -> Printf.fprintf ch "(%a) %a" print_term t print_term u
let print_term t =
  Printf.printf "%a\n%!" print_term (unbox (lift_term t))


(* Step by step evaluation function. *)
let rec cbn_step = function
  | App(Lam b, u) -> Some (subst b u)
  | App(t    , u) ->
      begin
        match cbn_step t with
        | None    -> None
        | Some t' -> Some (App(t',u))
      end
  | _             -> None

let rec eval t =
  match cbn_step t with
  | None    -> t
  | Some t' -> eval t'

(* marking application (as in the doc) *)

let mark t =
  let rec phi x = function
    Var(y) -> box_of_var y
  | App(u,v) -> app (app x (phi x u)) (phi x v)
  | Lam(f) -> vlam (binder_name f) (fun y -> phi x (subst f (Var y)))
  in unbox (lam "x" (fun x -> phi x t))

(* weak head normal form *)
let rec whnf = function
  App(t1,t2) as t0 -> (
    match (whnf t1) with
    | Lam f -> whnf (subst f t2)
    | t1' ->
       (* a small optimization here when the term is in whnf *)
       if t1' == t1 then t0 else App(t1', t2))
| t -> t

(* call by name normalisation, all step at once *)
let norm t = let rec fn t =
  match whnf t with
    Lam f ->
      vlam (binder_name f) (fun x -> fn (subst f (Var x)))
  | t ->
      let rec unwind = function
          Var(x) -> box_of_var x
        | App(t1,t2) -> app (unwind t1) (fn t2)
        | t -> assert false
      in unwind t
in unbox (fn t)

(* Tests. *)
let _ =
  print_term (unbox x);
  print_term (unbox y);
  print_term (unbox id);
  print_term (unbox fst);
  print_term (unbox delta);
  print_term (unbox omega);
  print_term (unbox fsty);
  print_term (eval (unbox fsty));
  print_term (unbox fstyx);
  print_term (eval (unbox fstyx))
