(** Experiment with hashconsing. *)

open Bindlib

type term = {mutable address : int; mutable key : int; data : term_aux}

and term_aux =
  | TVar of term var
  | TApp of term * term
  | TLam of (term, term) binder
  | HVar of int

type t = term

let canonical_var f =
  let n = if binder_occur f then binder_rank f + 1 else 0 in
  {address = -n; key = Hashtbl.hash (`HVar, n); data = HVar(n)}

let hash_binder f =
  Hashtbl.hash (binder_occur f, (subst f (canonical_var f)).key)

let hash_aux t =
  match t with
  | HVar(n)   -> Hashtbl.hash (`HVar, n)
  | TVar(x)   -> hash_var x
  | TApp(t,u) -> Hashtbl.hash (`TApp, t.key, u.key)
  | TLam(b)   -> Hashtbl.hash (`TLam, hash_binder b)

let hash r = r.key

let equal_binders b1 b2 =
  binder_rank b1 = binder_rank b2 &&
  binder_occur b1 = binder_occur b2 &&
  (subst b1 (canonical_var b1)).address =
  (subst b2 (canonical_var b2)).address

let equal_aux t1 t2 = t1 == t2 ||
  match (t1, t2) with
  | (HVar(n1)   , HVar(n2)   ) -> n1 = n2
  | (TVar(x1)   , TVar(x2)   ) -> eq_vars x1 x2
  | (TApp(t1,u1), TApp(t2,u2)) ->
      t1.address = t2.address && u1.address = u2.address
  | (TLam(b1)   , TLam(b2)   ) -> equal_binders b1 b2
  | (_          , _          ) -> false

let equal t1 t2 = equal_aux t1.data t2.data

module WLambda = Weak.Make(
  struct
    type t = term
    let hash = hash
    let equal = equal
  end)

let hashtbl = WLambda.create 1001

let get_addr =
  let counter = ref 1 in
  fun () -> let addr = !counter in incr counter; addr

let hash_cons data =
  let t = {address = get_addr (); key = hash_aux data; data} in
  try WLambda.find hashtbl t with Not_found -> WLambda.add hashtbl t; t

let re_hash_cons old =
  old.key <- hash_aux old.data;
  old.address <- get_addr ();
  try
    let res = WLambda.find hashtbl old in
    old.address <- get_addr (); res
  with Not_found -> WLambda.add hashtbl old; old

let rec print_in ctxt oc t =
  match t.data with
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

let mkfree : term var -> term = fun x ->
  hash_cons (TVar(x))

let var : term var -> term box =
  box_var

let lam : string -> (term box -> term box) -> term box = fun x f ->
  let x = Bindlib.new_var mkfree x in
  box_apply hash_cons (box_apply (fun x -> TLam x) (bind_var x (f (var x))))

let app : term box -> term box -> term box = fun t u ->
  box_apply hash_cons (box_apply2 (fun t u -> TApp(t,u)) t u)

let _App(t1,t2) = hash_cons(TApp(t1,t2))

let rec eval t =
  match t.data with
  | TVar(_)     -> failwith "open term"
  | HVar(_)     -> failwith "invalid term"
  | TLam(_)     -> t
  | TApp(t1,t2) ->
      let t1 = eval t1 in
      let t2 = eval t2 in
      let b = match t1.data with TLam(b) -> b | _ -> failwith "ill-typed" in
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

let _ = assert (idt.address = idt'.address)
let _ = assert (idt.data == idt'.data)

let _ = assert (delta.address = delta'.address &&
                  delta.data == delta'.data)

let idt'' = eval (_App(delta, _App(delta', idt)))

let _ =
  Printf.printf "%a\n%!" print_closed idt;
  Printf.printf "%a\n%!" print_closed idt';
  Printf.printf "%a\n%!" print_closed delta;
  Printf.printf "%a\n%!" print_closed delta';
  Printf.printf "%a\n%!" print_closed idt'';
  Printf.printf "%a\n%!" print_closed two;
  Printf.printf "%a\n%!" print_closed four;
  Printf.printf "%a\n%!" print_closed dog

let _ = assert (idt.address = idt''.address)
let _ = assert (idt.data == idt''.data)

let _ = assert (four.address = dog.address)
let _ = assert (four.data == dog.data)
