(* we open the library *)
open Bindlib

(* a possible representation for 
   second order predicate calculus *)

(* a structure to store the information 
   about a function symbol *)
type symbol = {name : string; arity : int }

(* the type of first order term *)
type term = 
    Fun of symbol * term array
  | TermVar of term variable

(* the type of second order formula *)
type form =
  Imply of form * form
| Forall1 of (term, form) binder
| Forall2 of int * (pred, form) binder
| FormVar of pred variable * term array

(* a predicate is a binder ! *)
and pred = (term, form) mbinder

let lam1 x = TermVar(x)

let lam2 arity x = 
  unbox (bind lam1 args(arity) in FormVar(^ (^x^), lift_array args^))

let rec print_term = function
    Fun(sy, ta) ->
      print_string sy.name;
      print_string "(";
      for i = 0 to sy.arity - 1 do
	print_term ta.(i);
	print_string (if i < sy.arity - 1 then "," else ")")
      done
  | TermVar(var) ->
      print_string (name_of var)

let rec print_form lvl = function
    Imply(f1, f2) ->
      if lvl > 0 then print_string "(";
      print_form 1 f1; print_string " => "; print_form lvl f2;
      if lvl > 0 then print_string ")";
  | Forall1 f ->
      match f with bind lam1 t  in g ->
	print_string "Forall1 ";
	print_string (name_of t);
	print_string " ";
	print_form 1 g
  | Forall2 (arity, f) ->
      match f with bind (lam2 arity) x in g ->
      print_string "Forall2 ";
      print_string (name_of x);
      print_string " ";
      print_form 1 g
  | FormVar(var,args) ->
      print_string (name_of var);
      print_string "(";
      let arity = Array.length args in
      for i = 0 to arity - 1 do
	print_term args.(i);
	print_string (if i < arity - 1 then "," else ")")
      done
      
let rec equal_term t t' = match t, t' with
    TermVar(x), TermVar(x') -> x == x'
  | Fun(sy,ta), Fun(sy',ta') when sy = sy' ->
      let r = ref true in
      for i = 0 to sy.arity - 1 do
	r := !r && equal_term ta.(i) ta'.(i)
      done;
      !r
  | _ -> false

let rec equal_form f f' = match f, f' with
    Imply(f,g), Imply(f',g') -> 
      equal_form f f' && equal_form g g'
  | Forall1(f), Forall1(f') ->
      match f with bind lam1 t in g ->
      equal_form g (subst f' (free_of t))
  | Forall2(arity,f), Forall2(arity',f') ->
      arity = arity' &&
      match f with bind (lam2 arity) x in g ->
      equal_form g (subst f' (free_of x))
  | FormVar(x,ta), FormVar(x',ta') ->
      x == x' && 
      let r = ref true in
      for i = 0 to Array.length ta - 1 do
	r := !r && equal_term ta.(i) ta'.(i)
      done;
      !r
  | _ -> false

let rec lift_term = function
    TermVar(x) -> bindbox_of x
  | Fun(sy,ta) -> Fun(^ (^sy^), lift_array (Array.map lift_term ta) ^)

let rec lift_form = function
    Imply(f1,f2) -> Imply(^ lift_form f1, lift_form f2 ^)
  | Forall1 f ->
      match f with bind lam1 t in g ->
       Forall1(^ bindvar t in lift_form g ^)
  | Forall2(arity,f) ->
      match f with bind (lam2 arity) x in g ->
      Forall2(^ (^arity^), bindvar x in lift_form g ^)
  | FormVar(x,args) ->
      mbind_apply (bindbox_of x) (lift_array (Array.map lift_term args))

type proof =
    Imply_intro of form * (proof,proof) binder
  | Imply_elim of proof * proof
  | Forall1_intro of (term, proof) binder
  | Forall1_elim of proof * term
  | Forall2_intro of int * (pred, proof) binder
  | Forall2_elim of proof * pred
  | Axiom of form * proof variable

let assume f x = Axiom(f,x)

let print_goal hyps concl = 
  List.iter (
  function
      Axiom(f, v) ->
	print_string (name_of v); print_string ":="; 
	print_form 0 (unbox (lift_form f)); 
	print_newline ()
    | _ ->
	failwith ("not an axiom")) hyps;
  print_string "  |- "; print_form 0 concl; print_newline ()

exception Bad_proof of string

let type_infer p = 
  let ctxt = empty_context in
  let rec fn hyps ctxt p = 
    let r = match p with 
      Imply_intro(f,p) ->
	match p with bind (assume f) ax for ctxt in p' ->
	Imply(^ lift_form f, fn (ax::hyps) ctxt p' ^)
    | Imply_elim(p1, p2) ->
	begin
	  let f1' = unbox (fn hyps ctxt p2) in
	  match unbox (fn hyps ctxt p1) with 
	    Imply(f1,f2) when equal_form f1 f1' -> lift_form f2
	  | Imply(f1,f2) ->
	      print_form 0 f1; print_string "<>"; print_form 0 f1';
	      print_newline ();
	      raise (Bad_proof("Imply"))
	  | _ ->
	      raise (Bad_proof("Imply"))
	end
    | Forall1_intro(p) ->
	match p with bind lam1 t for ctxt in p' ->
	Forall1(^ bindvar t in  fn hyps ctxt p'^)
    | Forall1_elim(p,t) ->
	begin
	  match unbox (fn hyps ctxt p) with
	    Forall1(f) -> lift_form (subst f t) 
	  | _ -> raise (Bad_proof("Forall1"))
	end
    | Forall2_intro(arity, f) ->
	match f with bind (lam2 arity) x for ctxt in p' ->
	Forall2(^ (^arity^),  bindvar x in fn hyps ctxt p' ^)
    | Forall2_elim(p,pred) ->
	begin
	  match unbox (fn hyps ctxt p) with
	    Forall2(arity, f) when arity = mbinder_arity pred ->
	      lift_form (subst f pred) 
	  | _ -> raise (Bad_proof("Forall2"))
	end
    | Axiom(f,_) ->
	lift_form f
    in
    print_goal (List.map free_of hyps) (unbox r); print_newline ();
    r
  in
  unbox (fn [] ctxt p)
	
let type_check p f =
  if not (equal_form (type_infer p) f) then raise (Bad_proof "conclusion")


(* Here is an example of second order predicate: leibniz equality *)
let leq = bind lam1 u(2) as [| "u"; "v" |] in
  Forall2 (^ (^1^), bind (lam2 1) pX as "X" in Imply(^ pX ^|^ [|^ u.(0) ^|], pX ^|^ [|^ u.(1) ^|] ^) ^)

let equal_transitive = unbox (
  Forall1(^ bind lam1 x in
    Forall1(^ bind lam1 y in
      Forall1(^ bind lam1 z in
	Imply(^ leq ^|^ [|^x; y^|],
          Imply(^ leq ^|^ [|^y; z^|], leq ^|^ [|^x; z^|] ^) ^) ^) ^) ^))

let equal_transitive_proof = unbox (
  Forall1_intro(^ bind lam1 x in
    Forall1_intro(^ bind lam1 y in
      Forall1_intro(^ bind lam1 z in
	(let f = leq ^|^ [|^x; y^|] in 
	Imply_intro(^ f , bind (assume (unbox f)) h1 in 
	  (let g = leq ^|^ [|^y; z^|] in 
	  Imply_intro(^ g,  bind (assume (unbox g)) h2 in 
	    Forall2_intro(^ (^1^), bind (lam2 1) pX as "X" in
	      (let p = pX ^|^ [|^ x ^|] in 
	      Imply_intro(^ p, bind (assume (unbox p)) h3 in
		(Imply_elim(^ Forall2_elim(^h2, pX^), 
		   Imply_elim(^ Forall2_elim(^h1, pX^), h3  
			      ^)^))^))^)^))^))^)^)^))
let _ =
  print_form 0 (type_infer equal_transitive_proof);
  type_check equal_transitive_proof equal_transitive;
  print_newline ()

