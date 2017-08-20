(* This example illustrates the use of [copy_var] to translate from one AST to
   another, without relying on an environment for variables. It also shows the
   difference between [vbind] and [bind_var]. *)

open Bindlib

(* Language A with its AST and smart constructors. *)
module A =
  struct
    type term =
      | A_Var of term var
      | A_Abs of (term, term) binder
      | A_App of term * term

    let mkfree x = A_Var(x)

    let var x = box_of_var (new_var mkfree x)
    let abs x f = box_apply (fun b -> A_Abs(b)) (vbind mkfree x f)
    let app = box_apply2 (fun t u -> A_App(t,u))
  end

(* Language B with its AST and smart constructors. *)
module B =
  struct
    type term =
      | B_Var of term var
      | B_Abs of (term, term) binder
      | B_App of term * term

    let mkfree x = B_Var(x)

    let var x = box_of_var (new_var mkfree x)
    let abs x f = box_apply (fun b -> B_Abs(b)) (vbind mkfree x f)
    let app = box_apply2 (fun t u -> B_App(t,u))

    (* Smart constructor using [bind_var]. *)
    let abs_var : term var -> term bindbox -> term bindbox =
      fun x t -> box_apply (fun b -> B_Abs(b)) (bind_var x t)
  end

(* Conversion functions between variables. *)
let a_to_b_var : A.term var -> B.term var =
  fun x -> copy_var x (name_of x) B.mkfree

let b_to_a_var : B.term var -> A.term var =
  fun x -> copy_var x (name_of x) A.mkfree

(* Translation function. *)
let translate : A.term -> B.term =
  let rec tr : A.term -> B.term bindbox = fun t ->
    match t with
    | A.A_Var(x)   -> box_of_var (a_to_b_var x)
    | A.A_Abs(b)   -> let f x = tr (subst b (A.A_Var(b_to_a_var x))) in
                      B.abs (binder_name b) f
    | A.A_App(t,u) -> B.app (tr t) (tr u)
  in
  fun t -> unbox (tr t)

(* NOTE for us to be able to write the translation function, [B.abs] should be
   defined with [vbind] and not with [bind]. Alternatively,  it is possible to
   rely on [bind_var], as shown in the following function. *)

(* Other translation function. *)
let translate' : A.term -> B.term =
  let rec tr : A.term -> B.term bindbox = fun t ->
    match t with
    | A.A_Var(x)   -> box_of_var (a_to_b_var x)
    | A.A_Abs(b)   -> let (x,t) = unbind A.mkfree b in
                      B.abs_var (a_to_b_var x) (tr t)
    | A.A_App(t,u) -> B.app (tr t) (tr u)
  in
  fun t -> unbox (tr t)

(* Tests. *)
let id_a = unbox (A.abs "x" (fun x -> box_of_var x))

let id_b  = translate  id_a
let id_b' = translate' id_a

let is_id_b t =
  match t with
  | B.B_Abs(b) ->
      let (x,t) = unbind B.mkfree b in
      let t = subst b (B.B_Var(x)) in
      begin
        match t with
        | B.B_Var(y) -> eq_vars x y
        | _          -> false
      end
  | _          -> false

let _ = assert (is_id_b id_b )
let _ = assert (is_id_b id_b')
