(** Second-order predicate calculus. This example requires a more advanced use
    of Bindlib, as the definition of second-order substitution is non-trivial,
    but it also demonstrates how powerful Bindlib can be. *)

(** The logic is parameterised over a signature, which defined various symbols
    for constants/functions with given arities. The type [symbol] represents a
    symbol with its name and its arity. *)
type symbol = { name : string ; arity : int }

(** Type of first order terms. *)
type term =
  | Var of term Bindlib.var
  (** First-order variable. *)
  | Fun of symbol * term array
  (** Symbol of applied to arguments (as many as the symbol's arity). *)

(** Type of formulas. *)
type form =
  | Imply of form * form
  (** Implication [A → B]. *)
  | Univ1 of (term, form) Bindlib.binder
  (** First-order universal quantification [∀₁ x.A]. *)
  | Univ2 of int * (pred, form) Bindlib.binder
  (** Second-order universal quantification [∀₂ X.A], where [X] is a predicate
      variable whose arity is given by the [int]. Note that we use yet another
      binder as the representation of predicates: this really is what make the
      quantifier "second-order". *)
  | FVari of pred Bindlib.var * term array
  (** Applied second-order variable [X(t1,...,tn)]. *)

(** To represent a predicate (i.e., a formula with [n] parameters), we rely on
    a multiple binder. Indeed, this is natural since a predicate variable must
    stand for a formula parameterised by terms. *)
and pred = (term, form) Bindlib.mbinder

(** Smart constructors. *)

let imply = Bindlib.box_apply2 (fun f g -> Imply(f,g))

let univ1 = Bindlib.box_apply (fun f -> Univ1(f))

let univ2 arity = Bindlib.box_apply (fun f -> Univ2(arity,f))

(** Injection of term variables into terms. *)
let fvar1 : term Bindlib.var -> term = fun x -> Var(x)

(** Injection of predicate variables (of the given arity) into predicates. The
    result of [fvar2 n X] is something like [λx1,...,xn.X(x1,...,xn)]. *)
let fvar2 : int -> pred Bindlib.var -> pred = fun arity x ->
  let xs = Array.init arity (Printf.sprintf "x%i") in
  let xs = Bindlib.new_mvar fvar1 xs in
  let ts = Bindlib.box_array (Array.map Bindlib.box_var xs) in
  let p = Bindlib.box_apply (fun ts -> FVari(x,ts)) ts  in
  Bindlib.unbox (Bindlib.bind_mvar xs p)

(** Lifting functions. *)

let rec box_term = function
  | Var(x)    -> Bindlib.box_var x
  | Fun(s,ta) -> let ta = Array.map box_term ta in
                 Bindlib.box_apply (fun a -> Fun(s,a)) (Bindlib.box_array ta)

let rec box_form = function
  | Imply(a,b) -> imply (box_form a) (box_form b)
  | Univ1(b)   -> univ1 (Bindlib.box_binder box_form b)
  | Univ2(a,b) -> univ2 a (Bindlib.box_binder box_form b)
  | FVari(x,a) -> let a = Bindlib.box_array (Array.map box_term a) in
                  Bindlib.box_apply2 Bindlib.msubst (Bindlib.box_var x) a

(* Pretty-printers. *)
let pp_array print_elt sep oc a =
  let f i e =
    let pref = if i = 0 then "" else sep in
    Printf.fprintf oc "%s%a" pref print_elt e
  in
  Array.iteri f a

let rec print_term : out_channel -> term -> unit = fun oc t ->
  let pp fmt = Printf.fprintf oc fmt in
  match t with
  | Var(x)   -> pp "%s" (Bindlib.name_of x)
  | Fun(s,a) -> pp "%s(%a)" s.name (pp_array print_term ", ") a

let rec print_form : Bindlib.ctxt -> out_channel -> form -> unit =
    fun ctxt oc f ->
  let pp fmt = Printf.fprintf oc fmt in
  match f with
  | Imply(a,b) -> pp "(%a) ⇒ (%a)" (print_form ctxt) a (print_form ctxt) b
  | Univ1(b)   -> let (x,f,ctxt) = Bindlib.unbind_in ctxt b in
                  pp "∀₁ %s.(%a)" (Bindlib.name_of x) (print_form ctxt) f
  | Univ2(_,b) -> let (x,f,ctxt) = Bindlib.unbind_in ctxt b in
                  pp "∀₂ %s.(%a)" (Bindlib.name_of x) (print_form ctxt) f
  | FVari(x,a) -> pp "%s(%a)" (Bindlib.name_of x) (pp_array print_term ", ") a

let print_form : out_channel -> form -> unit = fun oc f ->
  let ctxt = Bindlib.free_vars (box_form f) in
  print_form ctxt oc f

(* Equality functions. *)
let eq_arrays : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool =
  fun eq_elt a1 a2 ->
    let l = Array.length a1 in
    if Array.length a2 <> l then false else
    let eq = ref true in
    let i = ref 0 in
    while !i < l && !eq do
      eq := !eq && eq_elt a1.(!i) a2.(!i); incr i
    done; !eq

let rec equal_term t u =
  match (t, u) with
  | (Var(x)     , Var(y)     )              -> Bindlib.compare_vars x y = 0
  | (Fun(s1,ta1), Fun(s2,ta2)) when s1 = s2 -> eq_arrays equal_term ta1 ta2
  | _                                       -> false

let rec equal_form f g =
  match (f, g) with
  | (Imply(f1,g1), Imply(f2,g2)) -> equal_form f1 f2 && equal_form g1 g2
  | (Univ1(b1)   , Univ1(b2)   ) ->
      let x = fvar1 (Bindlib.new_var fvar1 "") in
      equal_form (Bindlib.subst b1 x) (Bindlib.subst b2 x)
  | (Univ2(a1,b1), Univ2(a2,b2)) when a1 = a2 ->
      let x = fvar2 a1 (Bindlib.new_var (fvar2 a1) "") in
      equal_form (Bindlib.subst b1 x) (Bindlib.subst b2 x)
  | (FVari(x,a1) , FVari(y,a2) ) ->
      Bindlib.compare_vars x y = 0 && eq_arrays equal_term a1 a2
  | _                            -> false

(** Type of natural deduction proof trees. All introduction rules are binders,
    and the introduction rule for implication binds a proof in a proof (axioms
    play the role of variables). *)

type proof =
  | Imply_i of form * (proof, proof) Bindlib.binder
  | Imply_e of proof * proof
  | Univ1_i of (term, proof) Bindlib.binder
  | Univ1_e of proof * term
  | Univ2_i of int * (pred, proof) Bindlib.binder
  | Univ2_e of proof * pred
  | Axiom   of form * proof Bindlib.var

let imply_i = Bindlib.box_apply2 (fun p q -> Imply_i(p,q))
let imply_e = Bindlib.box_apply2 (fun p q -> Imply_e(p,q))
let univ1_i = Bindlib.box_apply (fun p -> Univ1_i p)
let univ1_e = Bindlib.box_apply2 (fun p q -> Univ1_e(p,q))
let univ2_i arity = Bindlib.box_apply (fun p -> Univ2_i(arity,p))
let univ2_e = Bindlib.box_apply2 (fun p q -> Univ2_e(p,q))
let axiom f v = Axiom(f,v)

(** Function for printing a sequent: a list of named hypotheses represented as
    proof variables, and a conclusion. *)
let print_goal oc hyps concl =
  let pp fmt = Printf.fprintf oc fmt in
  let print_hyp h =
    match h with
    | Axiom(a,x) -> pp " %s : %a\n" (Bindlib.name_of x) print_form a
    | _          -> failwith "Not an axiom..."
  in
  List.iter print_hyp hyps;
  pp "------------------------------\n %a\n\n" print_form concl

exception Bad_proof of string

let type_infer p =
  let ctxt = Bindlib.empty_ctxt in
  let rec fn hyps ctxt p =
    let r =
      match p with
      | Imply_i(f,p) ->
         let (ax, ctxt) =
           Bindlib.new_var_in ctxt (axiom f) (Bindlib.binder_name p)
         in
         let tax = axiom f ax in
         imply (box_form f) (fn ((ax,tax)::hyps) ctxt (Bindlib.subst p tax))
      | Imply_e(p1,p2) ->
        begin
          let f1' = Bindlib.unbox (fn hyps ctxt p2) in
          match Bindlib.unbox (fn hyps ctxt p1) with
            Imply(f1,f2) when equal_form f1 f1' -> box_form f2
          | Imply(f1,_ ) ->
             Printf.eprintf "%a <> %a\n%!" print_form f1 print_form f1';
            raise (Bad_proof("Imply"))
          | _ ->
              raise (Bad_proof("Imply"))
        end
      | Univ1_i(p) ->
         let t,ctxt = Bindlib.new_var_in ctxt fvar1 (Bindlib.binder_name p) in
         univ1 (Bindlib.bind_var t (fn hyps ctxt (Bindlib.subst p (fvar1 t))))
      | Univ1_e(p,t) ->
        begin
          match Bindlib.unbox (fn hyps ctxt p) with
            Univ1(f) -> box_form (Bindlib.subst f t)
          | _ -> raise (Bad_proof("Univ1"))
        end
      | Univ2_i(arity,f) ->
         let t,ctxt =
           Bindlib.new_var_in ctxt (fvar2 arity) (Bindlib.binder_name f)
         in
         univ2 arity (Bindlib.bind_var t (
           fn hyps ctxt (Bindlib.subst f (fvar2 arity t))))
      | Univ2_e(p,pred) ->
         begin
          match Bindlib.unbox (fn hyps ctxt p) with
            Univ2(arity, f) when arity = Bindlib.mbinder_arity pred ->
              box_form (Bindlib.subst f pred)
          | _ -> raise (Bad_proof("Univ2"))
        end
      | Axiom(f,_) -> box_form f
    in
    print_goal stdout (List.map snd hyps) (Bindlib.unbox r);
    r
  in
  Bindlib.unbox (fn [] ctxt p)

let type_check p f =
  if not (equal_form (type_infer p) f) then
    raise (Bad_proof "Infered type does not match expected type.")

(** Example of second order predicate: leibniz equality *)
let leq =
  let uv = Bindlib.new_mvar fvar1 [| "u"; "v" |] in
  let tu = Bindlib.box_var uv.(0) in
  let tv = Bindlib.box_var uv.(1) in
  let x = Bindlib.new_var (fvar2 1) "X" in
  let tx = Bindlib.box_var x in
  Bindlib.bind_mvar uv @@
  univ2 1 @@ Bindlib.bind_var x @@
  imply (Bindlib.mbind_apply tx (Bindlib.box_array [|tu|]))
        (Bindlib.mbind_apply tx (Bindlib.box_array [|tv|]))

let equal_transitive =
  let x = Bindlib.new_var fvar1 "x" in
  let y = Bindlib.new_var fvar1 "y" in
  let z = Bindlib.new_var fvar1 "z" in
  let tx = Bindlib.box_var x in
  let ty = Bindlib.box_var y in
  let tz = Bindlib.box_var z in
  Bindlib.unbox @@
  univ1 @@ Bindlib.bind_var x @@
  univ1 @@ Bindlib.bind_var y @@
  univ1 @@ Bindlib.bind_var z @@
  imply (Bindlib.mbind_apply leq (Bindlib.box_array [|tx; ty|]))
        (imply (Bindlib.mbind_apply leq (Bindlib.box_array [|ty; tz|]))
               (Bindlib.mbind_apply leq (Bindlib.box_array [|tx; tz|])))

let equal_transitive_proof =
  let x = Bindlib.new_var fvar1 "x" in
  let y = Bindlib.new_var fvar1 "y" in
  let z = Bindlib.new_var fvar1 "z" in
  let tx = Bindlib.box_var x in
  let ty = Bindlib.box_var y in
  let tz = Bindlib.box_var z in
  Bindlib.unbox @@
  univ1_i @@ Bindlib.bind_var x @@
  univ1_i @@ Bindlib.bind_var y @@
  univ1_i @@ Bindlib.bind_var z @@
  let f = Bindlib.mbind_apply leq (Bindlib.box_array [|tx; ty|]) in
  let h1 = Bindlib.new_var (axiom (Bindlib.unbox f)) "h1" in
  imply_i f (Bindlib.bind_var h1 (
    let g = Bindlib.mbind_apply leq (Bindlib.box_array [|ty; tz|]) in
    let h2 = Bindlib.new_var (axiom (Bindlib.unbox g)) "h2" in
    imply_i g  (Bindlib.bind_var h2 (
      let pX = Bindlib.new_var (fvar2 1) "X" in
      univ2_i 1 (Bindlib.bind_var pX (
        let p =
          Bindlib.mbind_apply (Bindlib.box_var pX) (Bindlib.box_array [|tx|])
        in
        let h3 = Bindlib.new_var (axiom (Bindlib.unbox p)) "h3" in
        imply_i p (Bindlib.bind_var h3 (
          imply_e (univ2_e (Bindlib.box_var h2) (Bindlib.box_var pX))
                  (imply_e (univ2_e (Bindlib.box_var h1) (Bindlib.box_var pX))
                  (Bindlib.box_var h3))))))))))

let _ =
  Printf.printf "INFERING\n\n%!";
  let f = type_infer equal_transitive_proof in
  Printf.printf "TYPE: %a\n\n%!" print_form f;
  Printf.printf "CHECKING\n\n%!";
  type_check equal_transitive_proof equal_transitive;
  Printf.printf "OK\n%!";
