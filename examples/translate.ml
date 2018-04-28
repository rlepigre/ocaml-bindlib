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

    let var x = box_var x
    let abs x t = box_apply (fun b -> A_Abs(b)) (bind_var x t)
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

    let var x = box_var x
    let abs x t = box_apply (fun b -> B_Abs(b)) (bind_var x t)
    let app = box_apply2 (fun t u -> B_App(t,u))
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
    | A.A_Var(x)   -> box_var (a_to_b_var x)
    | A.A_Abs(b)   -> let (x,t) = unbind b in
                      B.abs (a_to_b_var x) (tr t)
    | A.A_App(t,u) -> B.app (tr t) (tr u)
  in
  fun t -> unbox (tr t)

(* Tests. *)
let id_a =
  let x = new_var A.mkfree "x" in
  unbox (A.abs x (A.var x))

let id_b = translate id_a
