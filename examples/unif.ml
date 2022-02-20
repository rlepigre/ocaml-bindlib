(** Higher-order unification. *)

type sort =
  | Atom  of string
  | Arrow of sort * sort

type cst = {cst_name : string; cst_sort : sort}

type term =
  | Cst of cst
  | Var of sort * term Bindlib.var
  | Abs of sort * (term, term) Bindlib.binder
  | App of term * term

  | UCst of int * sort
  | UVar of sort * term option ref

type tbox = term Bindlib.box
type tvar = term Bindlib.var
type tbinder = (term, term) Bindlib.binder

let mkfree : sort -> tvar -> term = fun s x -> Var(s,x)

let var : tvar -> tbox =
  Bindlib.box_var

let app : tbox -> tbox -> tbox =
  Bindlib.box_apply2 (fun t u -> App(t,u))

let abs : sort -> tbinder Bindlib.box -> tbox = fun s ->
  Bindlib.box_apply (fun t -> Abs(s,t))

let rec box_term : term -> tbox = fun t ->
  match t with
  | Var(_,x)  -> var x
  | Abs(s,b)  -> abs s (Bindlib.box_binder box_term b)
  | App(t,u)  -> app (box_term t) (box_term u)
  | UVar(_,{contents = Some t}) -> box_term t
  | _         -> Bindlib.box t

exception Type_Clash

let rec infer_type : term -> sort = fun t ->
  match t with
  | Cst(c)     -> c.cst_sort
  | Var(s,_)   -> s
  | Abs(s,f)   -> Arrow(s, infer_type (snd (Bindlib.unbind f)))
  | App(t1,t2) ->
      begin
        match infer_type t1 with
        | Arrow(s1,s2) when infer_type t2 = s1 -> s2
        | _                                    -> raise Type_Clash
      end
  | UVar(s,_)  -> s
  | UCst(_,s)  -> s

let new_uvar : sort -> term = fun s ->
  UVar(s, ref None)

let new_cst : sort -> term =
  let count = ref 0 in
  fun s -> incr count; UCst(!count, s)

let rec whnf t =
  match t with
  | App(t1,t2)                    ->
      begin
        match whnf t1 with
        | Abs(_,b) -> whnf (Bindlib.subst b t2)
        | t1'      -> if t1' != t1 then App(t1',t2) else t
      end
  | UVar(_, {contents = Some(t)}) -> whnf t
  | t                             -> t

let rec add_eqn eqns t1 t2 =
  let t1 = whnf t1 in
  let t2 = whnf t2 in
  match (t1, t2) with
  | (Abs(s,b1), Abs(_,b2)) ->
      let c = new_cst s in
      add_eqn eqns (Bindlib.subst b1 c) (Bindlib.subst b2 c)
  | (Abs(s,b1), _        ) ->
      let c = new_cst s in
      add_eqn eqns (Bindlib.subst b1 c) (App(t2, c))
  | (_        , Abs(s,b2)) ->
      let c = new_cst s in
      add_eqn eqns (App(t1,c)) (Bindlib.subst b2 c)
  | _                      ->
      (t1, t2) :: eqns

exception Clash

(* works on whnf only !*)
let head_args t =
  let rec fn n t =
    match t with
    | App(t1,_) -> fn (n+1) t1
    | _         -> (t, n)
  in
  fn 0 t

let rec new_avar sh l =
  match l with
  | (s, t) :: args -> app (new_avar (Arrow(s, sh)) args) t
  | []             -> Bindlib.box (new_uvar sh)

let imitate t1 t2 =
  let (h, arg2s) = head_args t2 in
  let (v, arg1s) = head_args t1 in
  let s1 = infer_type v in
  let s2 = infer_type h in
  let rec fn nargs t n =
    match (t, n) with
    | (sh1, 0) ->
        let rec gn h t' n' =
          match t',n' with
          | (sh2        , 0) -> if sh1 <> sh2 then
                                  failwith "bug in imitate";
                                h
          | (Arrow(s,s'), n) -> gn (app h (new_avar s nargs)) s' (n-1)
          | _                -> failwith "bug in imitate"
        in
        gn (Bindlib.box h) s2 arg2s
    | Arrow(s,s'), n when n > 0 ->
        let x = Bindlib.new_var (mkfree s) "x" in
        let t = fn ((s, var x) :: nargs) s' (n - 1) in
        abs s (Bindlib.bind_var x t)
    | _ ->
        failwith "bug in imitate"
  in
  Bindlib.unbox (fn [] s1 arg1s)

let rec right_depth s =
  match s with
  | Arrow(_,s2) -> 1 + right_depth s2
  | _           -> 0

type 'a obj =
  | Nothing   of int
  | Something of 'a

let project t1 t2 =
  let (v, arg1s) = head_args t1 in
  let s1 = infer_type v in
  let s2 = infer_type t2 in
  let rec fn projs i t n =
    match t, n with
    | _           , 0 -> projs
    | Arrow(s0,s'), n ->
        let k = right_depth s0 - right_depth s2 in
        if k < 0 then
          fn projs (i+1) s' (n-1)
        else
          let rec gn h nargs t' n' = match t', n' with
            | _, 0 ->
                let h = match h with
                        | Nothing _ -> failwith "bug in proj"
                        | Something x -> x
                in
                let rec kn h t'' n'' =
                  match t'', n'' with
                  | sh2        , 0 ->
                      if s2 <> sh2 then raise Exit; h
                  | Arrow(s,s'), i ->
                      kn (app h  (new_avar s nargs) ) s' (i - 1)
                  | _ -> failwith "bug in proj"
                in kn h s0 k
            | Arrow(s,s'), n ->
                let x = Bindlib.new_var (mkfree s) "x" in
                let t =
                  let h =
                    match h with
                    | Nothing(0) -> Something(var x)
                    | Nothing(i) -> Nothing(i - 1)
                    | _          -> h
                  in
                  gn h ((s, var x) :: nargs) s' (n - 1)
                in
                abs s (Bindlib.bind_var x t)
            | _ -> failwith "bug in proj"
          in
          (try
            let t = Bindlib.unbox (gn (Nothing i) [] s1 arg1s) in
            fn ((k,t)::projs) (i+1) s' (n-1)
          with Exit -> fn projs (i+1) s' (n-1))
    | _ -> failwith "bug in proj"
  in fn [] 0 s1 arg1s

exception Too_Deep

let re_whnf eqns =
  let fn acc ((t1, t2) as p) =
    let t1' = whnf t1 in
    let t2' = whnf t2 in
    if t1 == t1' && t2 == t2' then p :: acc else add_eqn acc t1' t2'
  in
  List.fold_left fn [] eqns

let rec unif_loop step eqns =
  (* works on whnf only !*)
  let rec rigid = function
    | App(t1,_) -> rigid t1
    | UVar(_,r) -> !r <> None
    | _         -> true
  in

  let get_one_rig_rig eqns =
    let rec fn acc l =
      match l with
      | ((t1,t2) as p) :: l ->
          if rigid t1 && rigid t2 then (p, acc @ l) else fn (p :: acc) l
      | []                  ->
          raise Not_found
    in
    fn [] eqns
  in

  let rec rig_rig eqns p =
    match p with
    | (App(t1,t2), App(t1',t2')) -> rig_rig (add_eqn eqns t2 t2') (t1, t1')
    | (Cst(c)    , Cst(c')     ) when c == c' -> eqns
    | (UCst(c,_) , UCst(c',_)  ) when c = c' -> eqns
    | _                          -> raise Clash
  in

  (* works only if all rigid rigid equations have been removed *)
  let rec get_one_rig_flex l =
    match l with
    | ((t1,t2) as p) :: l ->
        if rigid t2 then p
        else if rigid t1 then (t2, t1)
        else get_one_rig_flex l
    | [] -> raise Not_found
  in

  try
    let (eq, eqns) = get_one_rig_rig eqns in
    unif_loop step (rig_rig eqns eq)
  with Not_found ->
  try
    let (t1,t2) = get_one_rig_flex eqns in
    if step = 0 then raise Too_Deep;
    let (v, nb_args) = head_args t1 in
    match v with
    | UVar(_, ({contents = None} as v)) -> (
        try
          try
            if nb_args = 0 then
              let test_occur v t =
                let rec fn b t =
                  match whnf t with
                  | App(t1,t2) -> fn (fn b t1) t2
                  | Abs(s,f)   -> fn b (Bindlib.subst f (UCst(-1,s)))
                  | UVar(_, ({contents = None} as v')) ->
                            if v == v' then
                             if b then raise Exit
                            else raise Clash
                          else true
                  | _ -> false
                in
                ignore (fn false t)
              in
              test_occur v t2;
              v := Some t2;
              let eqns = re_whnf eqns in
              unif_loop step eqns
            else
              raise Exit
          with Exit ->
            try
              let l = project t1 t2 in
              let l = List.sort (fun (x,_) (y,_) -> compare x y) l in
              let rec fn l =
                match l with
                | [] -> raise Exit
                | (_,t)::l ->
                    try
                      v := Some t;
                      let eqns = re_whnf eqns in
                      unif_loop (step-1) eqns
                    with
                      Clash | Too_Deep -> v := None; fn l
              in
              fn l
            with Exit ->
              let t = imitate t1 t2 in
              v := Some t;
              let eqns = re_whnf eqns in
              unif_loop (step-1) eqns

        with e -> v := None; raise e)
    | _ -> failwith "bug in unif_loop"
  with Not_found -> ()

let unif step t1 t2 =
  let s1 = infer_type t1 and s2 = infer_type t2 in
  if s1 <> s2 then raise Type_Clash;
  unif_loop step (add_eqn [] t1 t2)

(* printing of term *)

let app_lvl = 2 and abs_lvl = 1 and ini_lvl = 0

let print_term t =
  let rec fn ctxt nv b t =
    match t with
    | App(t1,t2) ->
        if b >= app_lvl then print_string "(";
        fn ctxt nv abs_lvl t1;
        print_string " ";
        fn ctxt nv app_lvl t2;
        if b >= app_lvl then print_string ")"
    | Abs _ as t ->
        if b >= abs_lvl then print_string "(";
        print_string "\\";
        let rec gn ctxt nv t =
          match t with
          | Abs (s,f) ->
              let name = "x" ^ string_of_int nv in
	            let (v, ctxt) = Bindlib.new_var_in ctxt (mkfree s) name in
	            let t = Bindlib.subst f (mkfree s v) in
              print_string name;
              print_string " ";
              gn ctxt (nv + 1) t
          | _         ->
              print_string "-> ";
              fn ctxt nv ini_lvl t
        in
        gn ctxt nv t;
        if b >= abs_lvl then print_string ")"
    | Cst c ->
        print_string c.cst_name
    | UCst (n,_) ->
        print_string "!";
        print_int n
    | UVar(_, {contents = Some(t)}) ->
        fn ctxt nv app_lvl t
    | Var(_,x) ->
        print_string (Bindlib.name_of x)
    | _ ->
        print_string "?"
  in
  let ctxt = Bindlib.free_vars (box_term t) in
  fn ctxt 0 ini_lvl t

let alpha = Atom "α" and beta = Atom "β" and gamma = Atom "γ"
let c1 = Cst {cst_name = "c1"; cst_sort = alpha}
let c2 = Cst {cst_name = "c2"; cst_sort = alpha}
let idta =
  let x = Bindlib.new_var (mkfree alpha) "x" in
  Bindlib.unbox (abs alpha (Bindlib.bind_var x (var x)))
let f = Cst {cst_name = "f"; cst_sort = Arrow(alpha,alpha)}

let test n c1 c2 =
  try
    print_string "try  ";
    print_term c1; print_string " = "; print_term c2;
    print_newline ();
    unif n c1 c2;
    print_string "give ";
    print_term c1; print_string " = "; print_term c2;
    print_newline ();
    print_newline ();
  with
  | Clash    -> print_endline "Clash\n"
  | Too_Deep -> print_endline "Too_Deep\n"


let _ = test 10 c1 c1
let _ = test 10 c1 c2
let _ = test 10 (App(idta,c1)) (App(idta,c1))
let _ = test 10 (App(idta,c1)) (App(idta,c2))
let _ = test 10 (App(idta,c1)) (App(new_uvar(Arrow(alpha,alpha)),c1))
let _ = test 10 (App(idta,c1)) (App(new_uvar(Arrow(alpha,alpha)),c2))
let _ =
  let s = Arrow(alpha,(Arrow(Arrow(alpha,alpha),alpha))) in
  test 10 (App(idta,c1)) (App(App(new_uvar s, c1),idta))

let _ =
  let v = new_uvar(Arrow(alpha,alpha)) in
  test 10 (App(v,c1)) (App(f,App(v,c1)))

let _ =
  let v = new_uvar(Arrow(alpha,alpha)) in
  test 10 (App(v,App(v,c1))) (App(f,App(v,App(v,c1))))

let _ =
  let v = new_uvar(Arrow(alpha,Arrow(alpha,alpha))) in
  test 10 c1 (App(App(v,c1),c2))

let _ =
  let v = new_uvar(Arrow(alpha,Arrow(alpha,alpha))) in
  test 10 c2 (App(App(v,c1),c2))
