open Bindlib

type ty =
  | TyVar of ty var
  | TyArr of ty * ty
  | TyAll of (ty, ty) binder

type te =
  | TeVar of te var
  | TeAbs of ty * (te, te) binder
  | TeApp of te * te
  | TeLam of (ty, te) binder
  | TeSpe of te * ty

let mkfree_ty : ty var -> ty = fun x -> TyVar(x)
let mkfree_te : te var -> te = fun x -> TeVar(x)

let _TyVar : ty var -> ty box =
  box_var

let _TyArr : ty box -> ty box -> ty box =
  box_apply2 (fun a b -> TyArr(a,b))

let _TyAll : (ty,ty) binder box -> ty box =
  box_apply (fun f -> TyAll(f))

let _TeVar : te var -> te box =
  box_var

let _TeAbs : ty box -> (te,te) binder box -> te box =
  box_apply2 (fun a f -> TeAbs(a,f))

let _TeApp : te box -> te box -> te box =
  box_apply2 (fun t u -> TeApp(t,u))

let _TeLam : (ty,te) binder box -> te box =
  box_apply (fun f -> TeLam(f))

let _TeSpe : te box -> ty box -> te box =
  box_apply2 (fun t a -> TeSpe(t,a))

(** Head-normalisation function. *)
let rec hnf : te -> te =
  function
  | TeApp(t,u) ->
      begin
        let v = hnf u in
        match hnf t with
        | TeAbs(_,b) -> hnf (subst b v)
        | h          -> TeApp(h,v)
      end
  | TeSpe(t,a) ->
      begin
        match hnf t with
        | TeLam(b) -> hnf (subst b a)
        | h        -> TeSpe(h,a)
      end
  | t          -> t

module Examples = struct
  (* Creation of variables. *)
  let _X = new_var (fun x -> TyVar(x)) "X"
  let _Y = new_var (fun x -> TyVar(x)) "Y"
  let f  = new_var (fun x -> TeVar(x)) "f"
  let a  = new_var (fun x -> TeVar(x)) "a"

  (* Representation of the type X ⇒ Y. *)
  let _X_arr_Y : ty box =
    _TyArr (_TyVar _X) (_TyVar _Y)

  (* Representation of the type ∀X.∀Y.(X ⇒ Y) ⇒ X ⇒ Y. *)
  let appl_ty : ty box =
    _TyAll (bind_var _X (_TyAll (bind_var _Y (_TyArr _X_arr_Y _X_arr_Y))))

  (* Representation of the term ΛX.ΛY.λf:X⇒Y.λa:X.f a. *)
  let appl_te : te box =
    _TeLam (bind_var _X (_TeLam (bind_var _Y (
      _TeAbs _X_arr_Y (bind_var f (_TeAbs (_TyVar _X) (bind_var a (
        _TeApp (_TeVar f) (_TeVar a)))))))))
end

let rec lift_ty : ty -> ty box = fun a ->
  match a with
  | TyVar(x)   -> _TyVar x
  | TyArr(a,b) -> _TyArr (lift_ty a) (lift_ty b)
  | TyAll(f)   -> _TyAll (box_binder lift_ty f)

let rec lift_te : te -> te box = fun t ->
  match t with
  | TeVar(x)   -> _TeVar x
  | TeAbs(a,f) -> _TeAbs (lift_ty a) (box_binder lift_te f)
  | TeApp(t,u) -> _TeApp (lift_te t) (lift_te u)
  | TeLam(f)   -> _TeLam (box_binder lift_te f)
  | TeSpe(t,a) -> _TeSpe (lift_te t) (lift_ty a)

let update_ty : ty -> ty = fun a -> unbox (lift_ty a)
let update_te : te -> te = fun t -> unbox (lift_te t)

let rec print_ty : out_channel -> ty -> unit = fun oc a ->
  match a with
  | TyVar(x)   -> output_string oc (name_of x)
  | TyArr(a,b) -> Printf.fprintf oc "(%a) ⇒ (%a)" print_ty a print_ty b
  | TyAll(f)   -> let (x,a) = unbind f in
                  Printf.fprintf oc "∀%s.%a" (name_of x) print_ty a

let rec print_te : out_channel -> te -> unit = fun oc t ->
  match t with
  | TeVar(x)   -> output_string oc (name_of x)
  | TeAbs(a,f) -> let (x,t) = unbind f in
                  let x = name_of x in
                  Printf.fprintf oc "λ%s:%a.%a" x print_ty a print_te t
  | TeApp(t,u) -> Printf.fprintf oc "(%a) %a" print_te t print_te u
  | TeLam(f)   -> let (x,t) = unbind f in
                  Printf.fprintf oc "Λ%s.%a" (name_of x) print_te t
  | TeSpe(t,a) -> Printf.fprintf oc "(%a)<%a>" print_te t print_ty a

let print_ty : out_channel -> ty -> unit = fun oc a ->
  print_ty oc (update_ty a)

let print_te : out_channel -> te -> unit = fun oc t ->
  print_te oc (update_te t)

let rec nf : te -> te = fun t ->
  match t with
  | TeVar(_)   -> t
  | TeAbs(a,f) ->
      let (x,t) = unbind f in
      TeAbs(a, unbox (bind_var x (lift_te (nf t))))
  | TeApp(t,u) ->
      let u = nf u in
      begin
        match nf t with
        | TeAbs(_,f) -> nf (subst f u)
        | t          -> TeApp(t,u)
      end
  | TeLam(f)   ->
      let (x,t) = unbind f in
      TeLam(unbox (bind_var x (lift_te (nf t))))
  | TeSpe(t,a) ->
      begin
        match nf t with
        | TeLam(f) -> nf (subst f a)
        | t        -> TeSpe(t,a)
      end

let rec eq_ty : ty -> ty -> bool = fun a b -> a == b ||
  match (a, b) with
  | (TyVar(x1)   , TyVar(x2)   ) -> eq_vars x1 x2
  | (TyArr(a1,b1), TyArr(a2,b2)) -> eq_ty a1 a2 && eq_ty b1 b2
  | (TyAll(f1)   , TyAll(f2)   ) -> eq_binder eq_ty f1 f2
  | (_           , _           ) -> false

type ctxt = (te var * ty) list

let find_ctxt : te var -> ctxt -> ty option = fun x ctx ->
  try Some(snd (List.find (fun (y,_) -> eq_vars x y) ctx))
  with Not_found -> None

(* Raises [Not_found] in case of failure. *)
let rec infer : (te var * ty) list -> te -> ty = fun ctx t ->
  match t with
  | TeVar(x)   ->
      begin
        match find_ctxt x ctx with
        | None    -> failwith "[infer] variable not in context..."
        | Some(a) -> a
      end
  | TeAbs(a,f) ->
      let (x,t) = unbind f in
      let b = infer ((x,a)::ctx) t in
      TyArr(a,b)
  | TeApp(t,u) ->
      begin
        match infer ctx t with
        | TyArr(a,b) -> check ctx u a; b
        | _          -> failwith "[infer] expected arrow type..."
      end
  | TeLam(f)   ->
      let (x,t) = unbind f in
      let a = infer ctx t in
      TyAll(unbox (bind_var x (lift_ty a)))
  | TeSpe(t,b) ->
      begin
        match infer ctx t with
        | TyAll(f) -> subst f b
        | _        -> failwith "[infer] expected quantifier..."
      end

and check : (te var * ty) list -> te -> ty -> unit = fun ctx t a ->
  match (t, a) with
  | (TeVar(x)  , b         ) ->
      let a =
        match find_ctxt x ctx with
        | None    -> failwith "[check] variable not in context..."
        | Some(a) -> a
      in
      if not (eq_ty a b) then failwith "[check] type mismatch... (var)"
  | (TeAbs(c,f), TyArr(a,b)) ->
      if not (eq_ty c a) then failwith "[check] type mismatch... (abs)";
      let (x,t) = unbind f in
      check ((x,a)::ctx) t b
  | (TeApp(t,u), b         ) ->
      let a = infer ctx u in
      check ctx t (TyArr(a,b))
  | (TeLam(f1) , TyAll(f2) ) ->
      let (_,t,a) = unbind2 f1 f2 in
      check ctx t a
  | (TeSpe(t,b), a         ) ->
      begin
        match infer ctx t with
        | TyAll(f) ->
            let c = subst f b in
            if not (eq_ty c a) then failwith "[check] type mismatch... (spe)"
        | _        -> failwith "[infer] expected quantifier..."
      end
  | (_         , _         ) ->
      failwith "[check] not typable..."

let x  = new_var mkfree_te "x"
let y  = new_var mkfree_te "y"
let f  = new_var mkfree_te "f"
let _X = new_var mkfree_ty "X"
let _Y = new_var mkfree_ty "Y"

let fst =
  let fst =
    _TeLam (bind_var _X (_TeLam (bind_var _Y (
      _TeAbs (_TyVar _X) (bind_var x (_TeAbs (_TyVar _Y) (bind_var y (
        _TeVar x))))))))
  in
  unbox fst

let fst_fst_fst =
  let ty_fst = lift_ty (infer [] fst) in
  let fst = lift_te fst in
  let fst_fst_fst =
    _TeApp (_TeApp (_TeSpe (_TeSpe fst ty_fst) ty_fst) fst) fst
  in
  unbox fst_fst_fst

let app =
  let app =
    _TeLam (bind_var _X (_TeLam (bind_var _Y (
      _TeAbs (_TyArr (_TyVar _X) (_TyVar _Y)) (bind_var f (
        _TeAbs (_TyVar _X) (bind_var x (_TeApp (_TeVar f) (_TeVar x)))))))))
  in
  unbox app

let test_infer t =
  Printf.printf "⊢ %a : %a\n%!" print_te (nf t) print_ty (infer [] t)

let _ =
  test_infer fst;
  test_infer fst_fst_fst;
  test_infer app;
  Printf.printf "OK\n%!"
