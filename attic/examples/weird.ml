open Bindlib
open Lambda

(* marking application (as in the doc) *)

let mark t =
  let rec phi x = function
    Var(y) -> box_of_var y
  | App(u,v) -> app (app x (phi x u)) (phi x v)
  | Abs(f) -> vabs (binder_name f) (fun y -> phi x (subst f (Var y)))
  in unbox (abs "x" (fun x -> phi x t))

(* weak head normal form *)
let rec whnf = function
  App(t1,t2) as t0 -> (
    match (whnf t1) with
    | Abs f -> whnf (subst f t2)
    | t1' ->
       (* a small optimization here when the term is in whnf *)
       if t1' == t1 then t0 else App(t1', t2))
| t -> t

(* call by name normalisation, all step at once *)
let norm t = let rec fn t =
  match whnf t with
    Abs f ->
      vabs (binder_name f) (fun x -> fn (subst f (Var x)))
  | t ->
      let rec unwind = function
          Var(x) -> box_of_var x
        | App(t1,t2) -> app (unwind t1) (fn t2)
        | t -> assert false
      in unwind t
in unbox (fn t)
