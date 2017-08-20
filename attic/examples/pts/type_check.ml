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

    type stack_elt = A of expr | P1 | P2

    let rebuild h a =
      List.fold_left (fun e e' ->
		      match e' with
			A e' -> App(e,e')
		      | P1 -> Proj1 e
		      | P2 -> Proj2 e) h a

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
	    fn e1 (A e1'::args1) e2 args2
      	| _, _, App(e2,e2'), _ ->
	    fn e1 args1 e2 (A e2'::args2)
      	| Lambda(t1,e1), A e1'::args1, _, _ ->
	    fn (subst e1 e1') args1 e2 args2
      	| e1, args1, Lambda(t2,e2), A e2'::args2 ->
	    fn e1 args1 (subst e2 e2') args2
      	| Lambda(t1,e1), [], Lambda(t2,e2), [] ->
            let x = csthyp (binder_name e1) None t1 in
	    fn (subst e1 x) [] (subst e2 x) []
	| Pair(e1,e2), P1::args, e', args' ->
	   fn e1 args e' args'
	| Pair(e1,e2), P2::args, e', args' ->
	   fn e2 args e' args'
	| e, args, Pair(e1',e2'), P1::args' ->
	   fn e args e1' args'
	| e, args, Pair(e1',e2'), P2::args' ->
	   fn e args e2' args'
	| Pair(e1,e2), [], Pair(e1',e2'), [] ->
	   fn e1 [] e1' [] && fn e2 [] e2' []
	| Proj1(e), args, e', args' ->
	   fn e (P1::args) e' args'
	| Proj2(e), args, e', args' ->
	   fn e (P2::args) e' args'
 	| e, args, Proj1(e'), args' ->
	   fn e args e' (P1::args')
 	| e, args, Proj2(e'), args' ->
	   fn e args e' (P2::args')
     	| Pi(e1,e2), [], Pi(e1',e2'), [] ->
           let x = csthyp (binder_name e2) None e1 in
           fn e1 [] e1' [] && fn (subst e2 x) [] (subst e2' x) []
      	| Sigma(e1,e2), [], Sigma(e1',e2'), [] ->
            let x = csthyp (binder_name e2) None e1 in
            fn e1 [] e1' [] && fn (subst e2 x) [] (subst e2' x) []
      	| Atom(s1), [], Atom(s2), [] ->
	   s1 = s2
      	| Def({def_value = Some v1}), _, _, _ ->
	    fn v1 args1 e2 args2
      	| _, _, Def({def_value = Some v2}), _ ->
	    fn e1 args1 v2 args2
	| Def d1, _, Def d2, _ when d1.def_generation = d2.def_generation && d1.def_islocal && d2.def_islocal->
	   gn args1 args2
	| Goal(g1), args1, Goal(g2), args2 when g1 == g2 && gn args1 args2 -> true
	| Goal(_), args1, _, args2 | _, args1, Goal(_), args2 ->
	   print_string "|- "; print_expr (rebuild e1 args1); print_string " = "; print_expr (rebuild e2 args2);
	  print_newline ();
	  true
	| Pair(e1,e2), [], e', args' ->
	   fn e1 [] e' (args' @ [P1]) && fn e2 [] e' (args' @ [P2])
	| e, args, Pair(e1',e2'), [] ->
	   fn e (args @ [P1]) e1' [] && fn e (args @ [P2]) e2' []
	| _ -> false

      and gn args1 args2 =
	match args1, args2 with
	  [], [] -> true
	| (A e1::args1), (A e2::args2) -> fn e1 [] e2 [] && gn args1 args2
	| P1::args1, P1::args2 -> gn args1 args2
	| P2::args1, P2::args2 -> gn args1 args2
	| _ -> false

      in fn e1 [] e2 []

    let rec infer_sort args e =
      match e, args with

	Atom (s1), [] ->
	  get_axiom s1

      |	(Pi(e1,f2) | Sigma(e1,f2)), [] ->
	 let s1 = infer_sort [] e1 in
	 let x = new_var (constant_hyp e1) (binder_name f2) in
	 let g = subst f2 (free_of x) in
	 let s2 = infer_sort [] g in
	 get_rule s1 s2

      |	App(e1,e2), _ ->
          infer_sort (A e2::args) e1

      |	Lambda(t,f), (A a::args') ->
	 let _ = infer_sort [] t in
         type_check a t;
	 let x = new_var (constant_hyp t) (binder_name f) in
	 let g = subst f (free_of x) in
	 infer_sort args' g

      | Proj1(t), args ->
	 infer_sort (P1::args) t

      | Proj2(t), args ->
	 infer_sort (P2::args) t

      | Pair(t1,t2), P1::args ->
	 infer_sort args t1

      | Pair(t1,t2), P2::args ->
	 infer_sort args t2

      | Goal _, args ->
	 print_string "|- ";
	 print_expr (rebuild e args);
	 print_string " : SORT";
	 print_newline ();
	 failwith ""

      |	(Def { def_type = t0 }), args ->
	  begin
	    let rec unwind t args =
	      match whnf t, args with
	      	Atom(s), [] ->
		  s
	      | Pi(t,f), (A a::args') ->
	  	  type_check a t;
	  	  unwind (subst f (csthyp (binder_name f) (Some a) t)) args'
	      | Sigma(t,f), (P1::args') ->
		 unwind t args'
	      | Sigma(t,f), (P2::args') ->
		 unwind (subst f (csthyp (binder_name f) None t)) args'
	      | _ -> raise (Ill_sort e)
	    in unwind t0 args
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

	(Atom _ | Pi _ | Sigma _), _ ->
	  begin
	    match whnf t with
	      Atom(s2) ->
                let s1 = infer_sort args e in
	      	if not (s1 = s2) then raise (Ill_type(e,t))
	    | _ -> raise (Ill_type(e,t))
	  end

      |	App(e1,e2), _ ->
	  type_check' (A e2::args) e1 t

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

      |	Lambda(t',f), (A a::args') ->
	 let _ = infer_sort [] t' in
	 type_check' [] a t';
	 type_check' args' (subst f (csthyp (binder_name f) (Some a) t')) t

      | Pair(t1,t2), P1::args ->
	 type_check' args t1 t

      | Pair(t1,t2), P2::args ->
	 type_check' args t2 t

      | Pair(t1,t2), [] ->
	  begin
	    let _ = infer_sort [] t in
	    match whnf t with
	    | Sigma(t',ft) ->
	       type_check t1 t';
	       let v = csthyp (binder_name ft) (Some t1) t' in
	       type_check t2 (subst ft v)
	    | _ -> raise (Ill_type(e,t))
	  end

      | Proj1(t'), args ->
	 type_check' (P1::args) t' t

      | Proj2(t'), args ->
	 type_check' (P2::args) t' t

      | Goal _, args ->
	 print_string "|- ";
	 print_expr (rebuild e args);
	 print_string " : ";
	 print_expr t;
	 print_newline ()

      |	Def { def_type = t0 } as e0, args0 ->
	 (* FIXME: share with unwind above in infert sort *)
	  let rec unwind o args t' =
	    match whnf t', args with
	    | t', [] ->
	       if not (convertible t' t) then
		 raise (Ill_type(rebuild e0 args0,t))
	    | Def {def_value = Some t'}, args' ->
	       unwind o args' t'
	    | Pi(t',f), (A a::args') ->
	       type_check a t';
	       unwind (App(o,a)) args' (subst f (csthyp (binder_name f) (Some a) t'))
	    | Sigma(t,f), (P1::args') ->
	       unwind (Proj1 o) args' t
	    | Sigma(t,f), (P2::args') ->
	       unwind (Proj2 o) args' (subst f (csthyp (binder_name f) (Some (Proj1 o)) t))
	    | _ -> raise (Ill_type(rebuild e0 args0,t))
	  in unwind e0 args0 t0

      |	FVar _, _ ->
	 failwith "type_check does not accept FVar"

      | _ ->
	 assert false

end
