(** Experiment with hashconsing, using Conchon & Filliâtre's Hashcons
    library *)

open Bindlib
open Hashcons

type term = term_aux hash_consed

and term_aux =
  | TVar of term var
  | TApp of term * term
  | TLam of (term, term) binder
  | HVar of int

type t = term

module type HashedWithTbl = sig
  include HashedType
  type tbl
  val hashcons : t -> t hash_consed
  val init : int -> unit
end

module rec H : HashedWithTbl with type t = HH.key and type tbl = HH.t = struct
  type t = term_aux
  type tbl = HH.t
  let tbl = ref None
  let get_tbl () = match !tbl with
    | None -> assert false
    | Some tbl -> tbl

  let init n = if !tbl = None then tbl := Some (HH.create n)

  let hashcons t = HH.hashcons (get_tbl ()) t

  let canonical_var f =
    let n = if binder_occur f then binder_rank f + 1 else 0 in
    HH.hashcons (get_tbl ()) (HVar n)

  let hash_binder f =
    Hashtbl.hash (binder_occur f, (subst f (canonical_var f)).hkey)

  let hash t =
    match t with
    | HVar(n)   -> Hashtbl.hash (`HVar, n)
    | TVar(x)   -> hash_var x
    | TApp(t,u) -> Hashtbl.hash (`TApp, t.hkey, u.hkey)
    | TLam(b)   -> Hashtbl.hash (`TLam, hash_binder b)

  let equal_binders b1 b2 =
    binder_rank b1 = binder_rank b2 &&
      binder_occur b1 = binder_occur b2 &&
        (subst b1 (canonical_var b1)).tag =
          (subst b2 (canonical_var b2)).tag

  let equal t1 t2 =
    t1 == t2 ||
      match (t1, t2) with
      | (HVar(n1)   , HVar(n2)   ) -> n1 = n2
      | (TVar(x1)   , TVar(x2)   ) -> eq_vars x1 x2
      | (TApp(t1,u1), TApp(t2,u2)) ->
         t1.tag = t2.tag && u1.tag = u2.tag
      | (TLam(b1)   , TLam(b2)   ) -> equal_binders b1 b2
      | (_          , _          ) -> false
end

and HH : (S with type key = term_aux) = Make(H)

let _ = H.init 4096

let hashcons = H.hashcons

let mkfree : term var -> term = fun x ->
  hashcons (TVar(x))

let var : term var -> term box =
  box_var (* call mkfree and therefore hashcons when unboxing *)

let lam : string -> (term box -> term box) -> term box = fun x f ->
  let x = Bindlib.new_var mkfree x in
  box_apply hashcons (box_apply (fun x -> TLam x) (bind_var x (f (var x))))

let app : term box -> term box -> term box = fun t u ->
  box_apply hashcons (box_apply2 (fun t u -> TApp(t,u)) t u)

(* short cut for closed term application *)
let _App(t1,t2) = hashcons(TApp(t1,t2))

let rec eval t =
  match t.node with
  | TVar(_)     -> failwith "open term"
  | HVar(_)     -> failwith "invalid term"
  | TLam(_)     -> t
  | TApp(t1,t2) ->
      let t1 = eval t1 in
      let t2 = eval t2 in
      let b = match t1.node with TLam(b) -> b | _ -> failwith "ill-typed" in
      eval (subst b t2)

(* Tests. *)

let idt = unbox (lam "x" (fun x -> x))
let delta = unbox (lam "x" (fun x -> app x x))
let idt' = unbox (lam "y" (fun x -> x))
let delta' = unbox (lam "y" (fun x -> app x x))
let zero = unbox (lam "f" (fun _ -> lam "x" (fun x -> x)))
let succ = unbox (lam "n" (fun n -> lam "f" (fun f -> lam "x" (fun x ->
                  app (app n f) (app f x)))))
let two = unbox (lam "f" (fun f -> lam "x" (fun x -> app f (app f x))))
let add = unbox (lam "n" (fun n -> lam "m" (fun m -> lam "f" (fun f ->
                 lam "x" (fun x -> app (app n f) (app (app m f) x))))))

let dog = eval (_App(_App(_App(two,two),succ),zero))
let four = eval (_App(_App(_App(_App(add,two),two),succ),zero))

let _ = assert (idt.tag = idt'.tag)
let _ = assert (idt.node == idt'.node)

let _ = assert (delta.tag = delta'.tag &&
                  delta.node == delta'.node)

let idt'' = eval (_App(delta, _App(delta', idt)))

let rec print_in ctxt oc t =
  match t.node with
  | TVar(x)   ->
      Printf.fprintf oc "%s" (name_of x)
  | TLam(b)   ->
      let (x,t,ctxt) = Bindlib.unbind_in ctxt b in
      Printf.fprintf oc "λ%s.%a" (name_of x) (print_in ctxt) t
  | TApp(t,u) ->
      Printf.fprintf oc "(%a %a)" (print_in ctxt) t (print_in ctxt) u
  | HVar(n)   ->
      Printf.fprintf oc "{%i}" n

let print_closed = print_in empty_ctxt

let _ =
  Printf.printf "%a\n%!" print_closed idt;
  Printf.printf "%a\n%!" print_closed idt';
  Printf.printf "%a\n%!" print_closed delta;
  Printf.printf "%a\n%!" print_closed delta';
  Printf.printf "%a\n%!" print_closed idt'';
  Printf.printf "%a\n%!" print_closed two;
  Printf.printf "%a\n%!" print_closed four;
  Printf.printf "%a\n%!" print_closed dog

let _ = assert (idt.tag = idt''.tag)
let _ = assert (idt.node == idt''.node)

let _ = assert (four.tag = dog.tag)
let _ = assert (four.node == dog.node)
