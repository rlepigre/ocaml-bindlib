open Basic
open Bindlib
open Format

module Make(Pts: PtsType) =
  struct
    open Pts
    module PrintPts = Print.Make(Pts)
    open PrintPts
    open GlobalsPts
    open BasicPts
      
    exception Ill_type of expr * expr
    exception Ill_sort of expr
    exception Mismatch of expr * expr

    let rebuild h a = 
      List.fold_left (fun e e' -> App(e,e')) h a

    let convertible e1 e2 =
(*
      print_expr e1;
      print_string " = ";
      print_expr e2;
      print_newline ();
*)
      let rec fn e1 args1 e2 args2 = 
      	match e1, args1, e2, args2 with
      	  App(e1,e1'), _, _, _ ->
	    fn e1 (e1'::args1) e2 args2
      	| _, _, App(e2,e2'), _ ->
	    fn e1 args1 e2 (e2'::args2)
      	| Lambda(t1,e1), e1'::args1, _, _ ->
	    fn (subst e1 e1') args1 e2 args2
      	| e1, args1, Lambda(t2,e2), e2'::args2 ->
	    fn e1 args1 (subst e2 e2') args2
      	| Lambda(t1,e1), [], Lambda(t2,e2), [] ->
            let x = csthyp (binder_name e1) None t1 in
	    fn (subst e1 x) [] (subst e2 x) [] 
      	| Pi(e1,e2), [], Pi(e1',e2'), [] ->
            let x = csthyp (binder_name e2) None e1 in
            fn e1 [] e1' [] && fn (subst e2 x) [] (subst e2' x) []
      	| Atom(s1), [], Atom(s2), [] ->
	    s1 = s2
      	| Def({def_value = Some v1}), _, _, _ ->
	    fn v1 args1 e2 args2
      	| _, _, Def({def_value = Some v2}), _ ->
	    fn e1 args1 v2 args2
	| Def d1, _, Def d2, _ when d1.def_generation = d2.def_generation ->
	    gn args1 args2
	| _ -> false

      and gn args1 args2 = 
	match args1, args2 with
	  [], [] -> true
	| (e1::args1), (e2::args2) ->
	    fn e1 [] e2 [] && gn args1 args2
	| _ -> false

      in fn e1 [] e2 []

    let rec infer_sort args e =
      match e, args with

	Atom (s1), [] ->
	  get_axiom s1

      |	Pi(e1,f2), [] ->
          begin
	    let s1 = infer_sort [] e1 in
	    match f2 with bind (constant_hyp e1) x in g ->
	      let s2 = infer_sort [] g in
	      get_rule s1 s2
	  end
      |	App(e1,e2), _ ->
          infer_sort (e2::args) e1

      |	Lambda(t,f), (a::args') ->
	  begin 
	    let _ = infer_sort [] t in
            type_check a t;
	    match f with bind (valued_hyp t a) x in g ->
	      infer_sort args' g 
	  end
      |	Def d, args ->
	  begin
	    let rec unwind t args = 
	      match whnf t, args with
	      	Atom(s), [] -> 
		  s
	      | Pi(t,f), (a::args') ->
	  	  type_check a t;
	  	  unwind (subst f (csthyp (binder_name f) (Some a) t)) args'
	      | _ -> raise (Ill_sort e)
	    in unwind d.def_type args 
	  end

      |	FVar _, _ ->
	  failwith "type_check does not accept FVar"

      | _ -> raise (Ill_sort e)

    and type_check e t =
      type_check' [] e t
	    
    and type_check' args e t =
(*
      print_expr t;
      print_string " contains ";
      print_expr e;
      print_newline ();
*)
      match e, args with

	(Atom _ | Pi _), _ ->
	  begin
	    match whnf t with
	      Atom(s2) ->
                let s1 = infer_sort args e in
	      	if not (s1 = s2) then raise (Ill_type(e,t))
	    | _ -> raise (Ill_type(e,t))
	  end

      |	App(e1,e2), _ ->
	  type_check' (e2::args) e1 t 

      |	Lambda(t',f), [] ->
	  begin
	    let _ = infer_sort [] t' in
	    let _ = infer_sort [] t in
	    match whnf t with
	      Pi(t'',ft) ->
	      	if not (convertible t' t'') then raise (Ill_type(e,t));
	      	let v = csthyp (binder_name ft) None t' in
	      	type_check (subst f v) (subst ft v) 
	    | _ -> raise (Ill_type(e,t))
	  end

      |	Lambda(t',f), (a::args') ->
	  let _ = infer_sort [] t' in
	  type_check' args' (subst f (csthyp (binder_name f) (Some a) t')) t

      |	Def d as e0, args0 ->
          let t0 = t in 
	  let rec unwind args t' t =
	    match whnf t', args with
	      t', [] -> 
	  	if not (convertible t' t) then 
		  raise (Ill_type(rebuild e0 args0,t0))
	    | Pi(t',f), (a::args') ->
	  	type_check a t';
	  	unwind args' (subst f (csthyp (binder_name f) (Some a) t')) t
	    | _ -> raise (Ill_type(rebuild e0 args0,t0))
	  in unwind args0 d.def_type t0

      |	FVar _, _ ->
	  failwith "type_check does not accept FVar"

  end



