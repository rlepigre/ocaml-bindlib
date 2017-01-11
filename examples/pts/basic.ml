module StringCmp =
  struct
    type t = string
    let compare = compare
  end

module StringSet = Set.Make(StringCmp)
module StringMap = Map.Make(StringCmp)

exception Unbound of string

module type PtsType =
  sig
    type sort
    exception Ill_rule of sort * sort
    exception Ill_axiom of sort
    val get_axiom : sort -> sort
    val get_rule : sort -> sort -> sort
    val print_sort : sort -> unit
    val keywords : string list
    val parse_sort : sort Earley.grammar
  end

module Make(Pts: PtsType) =
  struct

    open Pts
    open Bindlib

    type expr =
	Atom of sort
      |	App of expr * expr
      |	Lambda of expr * (expr, expr) binder
      | Pair of expr * expr
      | Proj1 of expr | Proj2 of expr
      | Pi of expr * (expr, expr) binder
      | Sigma of expr * (expr, expr) binder
      |	Def of def

      | Goal of string
      |	FVar of expr variable  (* used by normalize *)

    and def =
  	{ def_name : string;
	  mutable def_value : expr option;
          def_type : expr;
	  def_generation : int;
 	  def_islocal : bool
	}

    let fVar x = FVar x
    let app = box_apply2 (fun t u -> App(t,u))
    let lambda name t u = box_apply2 (fun t u -> Lambda(t,u)) t (bind fVar name u)
    let pi name t u = box_apply2 (fun t u -> Pi(t,u)) t (bind fVar name u)
    let sigma name t u = box_apply2 (fun t u -> Sigma(t,u)) t (bind fVar name u)
    let pair = box_apply2 (fun t u -> Pair(t,u))
    let proj1 = box_apply (fun t -> Proj1 t)
    let proj2 = box_apply (fun t -> Proj2 t)

    let rec whnf = function
	App(e1,e2) as e0 ->
	  begin
	    match whnf e1 with
	      Lambda(_,e3) -> whnf (subst e3 e2)
            | e3 -> if e3 == e1 then e0 else App(e3,e2)
	  end
      | Proj1(e1) as e0 ->
	 begin
	   match whnf e1 with
	     Pair(e1,_) -> whnf e1
	   | e2 -> if e1 == e2 then e0 else Proj1 e2
	 end
      | Proj2(e1) as e0 ->
	 begin
	   match whnf e1 with
	     Pair(_,e1) -> whnf e1
	   | e2 -> if e1 == e2 then e0 else Proj2 e2
	 end
      |	Def {def_value = Some v} -> whnf v
                                   (* one should may be add an heuristic
                                      to avoid opening definition *)
      | e0 -> e0

    let gen_sym =
      let count = ref 0 in
      (fun () -> incr count; !count)

    let csthyp name v t =
      Def{ def_name=name;
	   def_value=v;
	   def_type=t;
	   def_generation=gen_sym ();
	   def_islocal = true
	 }

    let constant_hyp t x = csthyp (name_of x) None t

    let valued_hyp t v x = csthyp (name_of x) (Some v) t

    let rec equal e1 e2 = match e1, e2 with
      App(e1,e1'), App(e2,e2') ->
	equal e1 e2 && equal e1' e2'
    | Lambda(t1,f1), Lambda(t2,f2) ->
        let x = csthyp "" None t1 in
	equal t1 t2 && equal (subst f1 x) (subst f2 x)
    | Pi(t1,f1), Pi(t2,f2) ->
        let x = csthyp "" None t1 in
	equal t1 t2 && equal (subst f1 x) (subst f2 x)
    | Atom(s1), Atom(s2) ->
	s1 = s2
    | Def(d1), Def(d2) ->
	d1.def_generation = d2.def_generation
    | FVar(v1), FVar(v2) ->
	v1 == v2
    | _ -> false

    let normalize t =
      let rec fn t =
      	match whnf t with
  	  Lambda(t, f) ->
	    lambda (binder_name f) (fn t) (fun x -> fn (subst f (unbox x)))
      	| Pi(t, f) ->
	    pi (binder_name f) (fn t) (fun x -> fn (subst f (unbox x)))
      	| t ->
	    let rec unwind = function
	      	FVar x -> box_of_var x
	      | App(t1,t2) -> app (unwind t1) (fn t2)
	      | t -> box t
	    in unwind t
      in unbox (fn t)

    let lift_expr t =
      let rec fn t =
      	match t with
  	  Lambda(t, f) ->
	    lambda (binder_name f) (fn t) (fun x -> fn (subst f (unbox x)))
        | Pi(t, f) ->
	    pi (binder_name f) (fn t) (fun x -> fn (subst f (unbox x)))
        | Sigma(t, f) ->
	    sigma (binder_name f) (fn t) (fun x -> fn (subst f (unbox x)))
        | FVar x -> box_of_var x
	| App(t1,t2) -> app (fn t1) (fn t2)
	| Pair(t1,t2) -> pair (fn t1) (fn t2)
	| Proj1(t) -> proj1 (fn t)
	| Proj2(t) -> proj2 (fn t)
	| t -> box t
      in fn t

    let bArrow e1 e2 =
      pi "_" e1 (fun x -> e2)
  end
