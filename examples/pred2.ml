(* Implementation of the second-order predicate calculus. *)
open Bindlib

(* A structure representing a function symbol. *)
type symbol = { name : string ; arity : int }

(* The type of first order terms. *)
type term =
  | Var of term variable
  | Fun of symbol * term array

(* The type of formulas. *)
type form =
  (* Implication. *)
  | Imply of form * form
  (* First-order universal quantification. *)
  | Univ1 of (term, form) binder
  (* Second-order universal quantification. *)
  | Univ2 of int * (pred, form) binder
  (* Variable. *)
  | FVari of pred variable * term array

(* Predicate (implemented as a binder). *)
and pred = (term, form) mbinder

let unbind : ('a variable -> 'a) -> ('a,'b) binder -> 'a variable * 'b =
  fun fv b ->
    let x = new_var fv (binder_name b) in
    (x, subst b (free_of x))

let fvar1 x = Var x

let fvar2 ar x =
  let vs = Array.init ar (Printf.sprintf "x%i") in
  let f xs = box_apply2 (fun x y -> FVari(x,y)) (box x) (box_array xs) in
  unbox (mbind fvar1 vs f)

let unbind1 : (term, form) binder -> term variable * form =
  unbind (fun x -> Var x)

let unbind2 : int -> (pred, form) binder -> pred variable * form = fun a b ->
  unbind (fvar2 a) b

(* Pretty-printers. *)
let print_array print_elt sep och a =
  let f i e =
    let pref = if i = 0 then "" else sep in
    Printf.fprintf och "%s%a" pref print_elt e
  in
  Array.iteri f a

let rec print_term : out_channel -> term -> unit = fun och t ->
  match t with
  | Var(x)   -> output_string och (name_of x)
  | Fun(s,a) -> Printf.fprintf och "%s(%a)" s.name
                  (print_array print_term ", ") a

let rec print_form : out_channel -> form -> unit = fun och f ->
  match f with
  | Imply(a,b) -> Printf.fprintf och "(%a) ⇒ (%a)" print_form a print_form b
  | Univ1(b)   -> let (x,f) = unbind1 b in
                  Printf.fprintf och "∀₁%s.(%a)" (name_of x) print_form f
  | Univ2(a,b) -> let (x,f) = unbind2 a b in
                  Printf.fprintf och "∀₂%s.(%a)" (name_of x) print_form f
  | FVari(x,a) -> Printf.fprintf och "%s(%a)" (name_of x)
                    (print_array print_term ", ") a

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
  | (Var(x)     , Var(y)     )              -> compare_variables x y = 0
  | (Fun(s1,ta1), Fun(s2,ta2)) when s1 = s2 -> eq_arrays equal_term ta1 ta2
  | _                                       -> false

let rec equal_form f g =
  match (f, g) with
  | (Imply(f1,g1), Imply(f2,g2)) -> equal_form f1 f2 && equal_form g1 g2
  | (Univ1(b1)   , Univ1(b2)   ) ->
      let x = free_of (new_var fvar1 "") in
      equal_form (subst b1 x) (subst b2 x)
  | (Univ2(a1,b1), Univ2(a2,b2)) when a1 = a2 ->
      let x = free_of (new_var (fvar2 a1) "") in
      equal_form (subst b1 x) (subst b2 x)
  | (FVari(x,a1), FVari(y,a2)) ->
      compare_variables x y = 0 && eq_arrays equal_term a1 a2
  | _                          -> false

(* Lifting functions. *)
let rec box_term = function
  | Var(x)    -> box_of_var x
  | Fun(s,ta) -> let ta = Array.map box_term ta in
                 box_apply (fun a -> Fun(s,a)) (box_array ta)

let rec box_form = function
  | Imply(a,b) -> box_apply2 (fun a b -> Imply(a,b)) (box_form a) (box_form b)
  | Univ1(b)   ->
      let (x,a) = unbind1 b in
      box_apply (fun b -> Univ1 b) (bind_var x (box_form a))
  | Univ2(a,b) ->
      let (x,f) = unbind2 a b in
      box_apply (fun b -> Univ2(a,b)) (bind_var x (box_form f))
  | FVari(x,a) ->
      mbind_apply (box_of_var x) (box_array (Array.map box_term a))

(* Type of proof trees. *)
type proof =
  | Imply_i of form * (proof, proof) binder
  | Imply_e of proof * proof
  | Univ1_i of (term, proof) binder
  | Univ1_e of proof * term
  | Univ2_i of int * (pred, proof) binder
  | Univ2_e of proof * pred
  | Axiom   of form * proof variable

exception Bad_proof of string

let print_goal och hyps concl =
  let print_hyp och = function
    | Axiom(a,x) -> Printf.fprintf och " %s : %a\n" (name_of x) print_form a
    | _          -> failwith "Not an axiom..."
  in
  List.iter (print_hyp och) hyps;
  output_string och "----------------------------------------\n";
  Printf.fprintf och " %a\n" print_form concl

(* FIXME from here. *)

let assume f x = Axiom(f,x)

let type_infer p = 
  let ctxt = empty_context in
  let rec fn hyps ctxt p = 
    let r = match p with 
      | Imply_i(f,p) ->
          assert false (*
          (match p with bind (assume f) ax for ctxt in p' ->
            Imply(^ box_form f, fn (ax::hyps) ctxt p' ^) ) *)
      | Imply_e(p1,p2) ->
          assert false (*
        begin
          let f1' = unbox (fn hyps ctxt p2) in
          match unbox (fn hyps ctxt p1) with 
            Imply(f1,f2) when equal_form f1 f1' -> box_form f2
          | Imply(f1,f2) ->
              print_form 0 f1; print_string "<>"; print_form 0 f1';
              print_newline ();
              raise (Bad_proof("Imply"))
          | _ ->
              raise (Bad_proof("Imply"))
        end *)
      | Univ1_i(p) ->
          assert false (*
        (match p with bind fvar1 t for ctxt in p' ->
        Univ1(^ bindvar t in  fn hyps ctxt p'^) ) *)
      | Univ1_e(p,t) ->
          assert false (*
        begin
          match unbox (fn hyps ctxt p) with
            Univ1(f) -> box_form (subst f t) 
          | _ -> raise (Bad_proof("Univ1"))
        end *)
      | Univ2_i(arity,f) ->
          assert false (*
        (match f with bind (fvar2 arity) x for ctxt in p' ->
        Univ2(^ (^arity^),  bindvar x in fn hyps ctxt p' ^) ) *)
      | Univ2_e(p,pred) ->
          assert false (*
        begin
          match unbox (fn hyps ctxt p) with
            Univ2(arity, f) when arity = mbinder_arity pred ->
              box_form (subst f pred) 
          | _ -> raise (Bad_proof("Univ2"))
        end *)
      | Axiom(f,_) -> box_form f
    in print_goal stdout (List.map free_of hyps) (unbox r); r
  in
  unbox (fn [] ctxt p)
        
let type_check p f =
  if not (equal_form (type_infer p) f) then
    raise (Bad_proof "Infered type does not match expected type.")


(*
(* Here is an example of second order predicate: leibniz equality *)
let leq = bind fvar1 u(2) as [| "u"; "v" |] in
  Univ2 (^ (^1^), bind (fvar2 1) pX as "X" in Imply(^ pX ^|^ [|^ u.(0) ^|], pX ^|^ [|^ u.(1) ^|] ^) ^)

let equal_transitive = unbox (
  Univ1(^ bind fvar1 x in
    Univ1(^ bind fvar1 y in
      Univ1(^ bind fvar1 z in
        Imply(^ leq ^|^ [|^x; y^|],
          Imply(^ leq ^|^ [|^y; z^|], leq ^|^ [|^x; z^|] ^) ^) ^) ^) ^) )

let equal_transitive_proof = unbox (
  Univ1_i(^ bind fvar1 x in
    Univ1_i(^ bind fvar1 y in
      Univ1_i(^ bind fvar1 z in
        (let f = leq ^|^ [|^x; y^|] in 
        Imply_i(^ f , bind (assume (unbox f)) h1 in 
          (let g = leq ^|^ [|^y; z^|] in 
          Imply_i(^ g,  bind (assume (unbox g)) h2 in 
            Univ2_i(^ (^1^), bind (fvar2 1) pX as "X" in
              (let p = pX ^|^ [|^ x ^|] in 
              Imply_i(^ p, bind (assume (unbox p)) h3 in
                (Imply_e(^ Univ2_e(^h2, pX^), 
                   Imply_e(^ Univ2_e(^h1, pX^), h3  
                              ^) ^) ) ^) ) ^) ^) ) ^) ) ^) ^) ^) )
let _ =
  print_form 0 (type_infer equal_transitive_proof);
  type_check equal_transitive_proof equal_transitive;
  print_newline ()
*)
