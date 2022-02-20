open Bindlib
open Timed (* We use timed references for backtracking. *)

(** Term of the λ-calculus with meta-variables. *)
type term =
  | Var of term var
  (* Variable. *)
  | Lam of (term, term) binder
  (** Abstraction. *)
  | App of term * term
  (** Application. *)
  | Mta of meta * term array
  (** Meta-variable with its environment. *)

(** Meta-variable. *)
and meta =
  { mname : string
  (** Name of the meta-variable. *)
  ; ctxte : string array
  (** Context of the meta-variable. *)
  ; value : (term, term) mbinder option Timed.ref
  (** Value of the meta-variable if it is set. *) }

type tbox = term box

(* Smart constructors *)
let var : term var -> term
  = fun v -> Var v
let app : tbox -> tbox -> tbox
  = box_apply2 (fun u v -> App(u,v))
(* choose bindings from an existing var (not using bind or vbind *)
let lam : term var -> tbox -> tbox
  = fun v t -> box_apply (fun b -> Lam b) (bind_var v t)
let mta : meta -> tbox array -> tbox
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
  | Lam b      -> let (x, t, ctxt) = unbind_in ctxt b in
                  if wrap then Printf.fprintf ch "(";
                  Printf.fprintf ch "λ%s.%a" (name_of x) (print_term false ctxt) t;
                  if wrap then Printf.fprintf ch ")";
  | App(t,u)   -> Printf.fprintf ch "%a %a" pterm t wterm u
  | Mta(mta,a) -> Printf.fprintf ch "%s[%a]" mta.mname parray a

let print_term = print_term false

(* normalisation function *)
let rec whnf : term -> term = fun t ->
  match t with
  | App(t1,t2) as t0 -> (
    match (whnf t1) with
    | Lam f -> whnf (subst f t2)
    | t1' ->
       (* a small optimization here when the term is in whnf *)
       if t1' == t1 then t0 else App(t1', t2))
  | Mta({ value = r; _}, a) ->
      begin
        match !r with
        | Some f -> whnf (msubst f a)
        | None   -> t
      end
  | _ -> t

let rec norm : term -> tbox = fun t ->
  match whnf t with
  | Lam(f) ->
     let (x,t) = unbind f in
     lam x (norm t)
  | t      ->
  let rec unwind t =
    match t with
    | Var(x)   -> box_var x
    | App(t,u) -> app (unwind t) (norm u)
    | Mta(m,a) -> mta m (Array.map norm a)
    | Lam(_)   -> assert false
  in
  unwind t
