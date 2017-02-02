open Bindlib
open Timed (* timed ref for backtracking *)

type term =
  | App of term * term
  | Lam of (term, term) binder
  | Mta of meta * term array (* meta variable with their arguments *)
  | Var of term var

and meta = { mname : string
           ; ctxte : string array (* the context of the meta var *)
           ; value : (term, term) mbinder option ref (* its value if set *) }
(* NOTE: the value must be a ref, for undo to work with Timed *)

type bterm = term bindbox

(* Smart constructors *)
let var : term var -> term
  = fun v -> Var v
let app : bterm -> bterm -> bterm
  = box_apply2 (fun u v -> App(u,v))
(* choose bindings from an existing var (not using bind or vbind *)
let lam : term var -> bterm -> bterm
  = fun v t -> box_apply (fun b -> Lam b) (bind_var v t)
let mta : meta -> bterm array -> bterm
  = fun m a -> box_apply (fun a -> Mta(m,a)) (box_array a)

(* Printing function. *)
let rec print_term wrap ctxt ch =
  let pterm = print_term false ctxt in
  let wterm = print_term true ctxt in
  let parray ch =
    Array.iteri (fun i t ->
        Printf.fprintf ch "%s%a"
                       (if i > 0 then "," else "")
                       pterm t)
  in
  function
  | Var x      -> Printf.fprintf ch "%s" (name_of x)
  | Lam b      -> let (x, t, ctxt) = unbind_in ctxt var b in
                  if wrap then Printf.fprintf ch "(";
                  Printf.fprintf ch "λ%s.%a" (name_of x) (print_term false ctxt) t;
                  if wrap then Printf.fprintf ch ")";
  | App(t,u)   -> Printf.fprintf ch "%a %a" pterm t wterm u
  | Mta(mta,a) -> Printf.fprintf ch "%s[%a]" mta.mname parray a

let print_term = print_term false

(* normalisation function *)
let rec whnf:term -> term = function
  | App(t1,t2) as t0 -> (
    match (whnf t1) with
    | Lam f -> whnf (subst f t2)
    | t1' ->
       (* a small optimization here when the term is in whnf *)
       if t1' == t1 then t0 else App(t1', t2))
  | Mta({ value = { contents = Some f}}, a) -> whnf (msubst f a)
  | t -> t

let norm (t:term) : bterm =
  let rec fn t =
    match whnf t with
    | Lam f ->
       let (x,t) = unbind var f in
       lam x (fn t)
    | t ->
       let rec unwind = function
         | Var(x) -> box_of_var x
         | App(t1,t2) -> app (unwind t1) (fn t2)
         | Mta(t,args) -> mta t (Array.map fn args)
         | Lam _ -> assert false
       in unwind t
  in fn t (* no unbox *)
