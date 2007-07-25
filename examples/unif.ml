(* Unification d'ordre supérieur *)

open Bindlib
open Format

type atom_sort = string

type sort =
  Atom of atom_sort
| Arrow of sort * sort

type cst = {cst_name : string; cst_sort : sort}

type term = 
  App of term * term
| Abs of sort * (term, term) binder
| Cst of cst

| UCst of int * sort
| UVar of value ref

| FVar of term variable * sort

and value = Unknown of sort | Known of term

let fVar s v = FVar(v,s)

exception Type_Clash

let rec infer_type = function
  Cst c -> c.cst_sort
| FVar (_,s) -> s
| Abs(s,f) -> Arrow(s, infer_type (subst f (UCst (0,s))))
| App(t1,t2) -> (
    match infer_type t1 with
      Arrow(s1,s2) when infer_type t2 = s1 -> s2
    | _ -> raise Type_Clash)
| UVar{contents = Unknown s} -> s
| UVar{contents = Known t} -> infer_type t
| UCst(_,s) -> s


let new_var s = UVar (ref (Unknown s))

let new_cst =
  let count = ref 0 in
  (fun s -> incr count; UCst(!count,s))


(* weak head normal form *)

let rec whnf = function
  App(t1,t2) as t0 -> (
    match (whnf t1) with
      Abs (s,f) -> whnf (subst f t2)
    | t1' -> if t1' != t1 then App(t1',t2) else t0)
| UVar{contents = Known t} -> whnf t
| t -> t


let rec add_eqn eqns t t' = 
  let t1 = whnf t and
      t2 =  whnf t' in
  match t1, t2 with
    Abs(s,f), Abs(s',f') -> 
      let c = new_cst s in add_eqn eqns (subst f c) (subst f' c)
  | Abs(s,f), t -> 
      let c = new_cst s in add_eqn eqns (subst f c) (App(t,c))
  | t, Abs(s,f) -> 
      let c = new_cst s in add_eqn eqns (App(t,c)) (subst f c)
  | _ -> (t1,t2)::eqns


(* works on whnf only !*)
let rec rigid = function
  App(t1,t2) -> rigid t1
| UVar{contents = Unknown _} -> false
| _ -> true


let rec get_one_rig_rig eqns = 
  let rec fn acc = function
  [] -> raise Not_found
| (t1,t2) as tpl::l -> 
    if rigid t1 & rigid t2 then tpl,(acc@l)
    else fn (tpl::acc) l
  in fn [] eqns

(* works only if all rigid rigid equations have been removed *)
let rec get_one_rig_flex = function
  [] -> raise Not_found
| (t1,t2) as tpl::l -> 
    if rigid t2 then tpl
    else if rigid t1 then (t2,t1)
    else get_one_rig_flex l

                   
exception Clash

let rec rig_rig eqns = function
  App(t1,t2), App(t1',t2') -> rig_rig (add_eqn eqns t2 t2') (t1, t1')
| Cst c, Cst c' when c == c' -> eqns
| UCst (c,_), UCst (c',_) when c = c' -> eqns
| _ -> raise Clash



(* works on whnf only !*)
let rec head_args t = 
  let rec fn n = function
      App(t1,t2) -> fn (n+1) t1
    | t -> t, n
  in fn 0 t

let rec new_avar sh = function
  [] -> (^new_var sh^)
| (s,t)::args -> App(^new_avar (Arrow(s,sh)) args, t^)

 
let imitate t1 t2 =
  let h, arg2s = head_args t2 in
  let v, arg1s = head_args t1 in
  let s1 = infer_type v and s2 = infer_type h in
  let rec fn nargs t n = match t,n with
    | sh1, 0 -> 
      	let rec gn h t' n' = match t',n' with
    	| sh2, 0 -> 
            if sh1 <> sh2 then failwith "bug in imitate";
            h
    	| Arrow(s,s'), n ->
            gn (App(^h, new_avar s nargs^) ) s' (n-1)
      	| _ -> failwith "bug in imitate"
      	in gn (^h^) s2 arg2s 
    | Arrow(s,s'), n when n > 0 -> 
      	Abs(^ (^s^), bind (fVar s) x in fn ((s,x)::nargs) s' (n-1) ^) 
    | _ -> failwith "bug in imitate"
  in unbox(fn [] s1 arg1s)

let rec right_depth = function
  Arrow(s,s') -> 1 + right_depth s' 
| _ -> 0


type 'a obj = Nothing of int | Something of 'a


let test_occur v t = 
  let rec fn b t = 
    match whnf t with
      App(t1,t2) -> fn (fn b t1) t2
    | Abs(s,f) -> fn b (subst f (UCst(-1,s)))
    | UVar {contents = Unknown _} as v' -> 
      	if v == v' then if b then raise Exit else raise Clash
      	else true
    | _ -> false
  in fn false t

let project t1 t2 = 
  let v, arg1s = head_args t1 in
  let s1 = infer_type v and s2 = infer_type t2 in
  let rec fn projs i t n = match t, n with
    | sh1, 0 -> projs
    | Arrow(s0,s'), n -> 
      	let k = right_depth s0 - right_depth s2 in
      	if k < 0 then fn projs (i+1) s' (n-1) else
    	let rec gn h nargs t' n' = match t', n' with
    	| _, 0 -> 
	    let h = match h with
              Nothing _ -> failwith "bug in proj"
	    | Something x -> x in
	    let rec kn h t'' n'' = match t'', n'' with
      	    | sh2, 0 -> 
            	if s2 <> sh2 then raise Exit;
            	h
	    | Arrow(s,s'), i ->
          	kn (App(^h, new_avar s nargs^) ) s' (i - 1)
	    | _ -> failwith "bug in proj"
            in kn h s0 k
    	| Arrow(s,s'), n -> 
	    Abs(^ (^s^), bind (fVar s) x in
              let h = match h with
          	Nothing 0 -> Something x
              | Nothing i -> Nothing (i-1)
              | _ -> h in
              gn h ((s,x)::nargs) s' (n-1) ^) 
    	| _ -> failwith "bug in proj"
        in 
	(try 
          let t = unbox (gn (Nothing i) [] s1 arg1s) in
          fn ((k,t)::projs) (i+1) s' (n-1)
	with 
	  Exit -> fn projs (i+1) s' (n-1))
    | _ -> failwith "bug in proj"
  in fn [] 0 s1 arg1s

exception Too_Deep

let re_whnf eqns =
  List.fold_left (fun acc (t1,t2 as tuple) ->
    let t1' = whnf t1 and t2' = whnf t2 in
    if t1 == t1' && t2 == t2' then tuple::acc else add_eqn acc t1' t2') 
    [] eqns
    
let rec unif_loop step eqns =
  try 
    let eq, eqns = get_one_rig_rig eqns in
    unif_loop step (rig_rig eqns eq)
  with Not_found -> try
    let (t1,t2) = get_one_rig_flex eqns in
    if step = 0 then raise Too_Deep;
    let v, nb_args = head_args t1 in
    match v with
      UVar ({contents = Unknown s} as value) -> ( 
        try try
          if nb_args = 0 then begin
            let _ = test_occur v t2 in
            value := Known t2;
	    let eqns = re_whnf eqns in
            unif_loop step eqns
          end else raise Exit
        with Exit -> try
          let l = project t1 t2 in
          let l = Sort.list (fun (x,_) (y,_) -> x < y) l in
          let rec fn = function
            [] -> raise Exit
          | (_,t)::l ->
            try 
              value := Known t;
	      let eqns = re_whnf eqns in
              unif_loop (step-1) eqns
            with 
              Clash | Too_Deep -> value := Unknown s; fn l
	  in fn l 
        with Exit -> 
          let t = imitate t1 t2 in
          value := Known t;
	  let eqns = re_whnf eqns in
          unif_loop (step-1) eqns
        with e -> 
          value := Unknown s; raise e)
    | _ -> failwith "bug in unif_loop" 
  with Not_found -> ()


let unif step t1 t2 = 
  let s1 = infer_type t1 and s2 = infer_type t2 in
  if s1 <> s2 then raise Type_Clash;
  unif_loop step (add_eqn [] t1 t2)



(* printing of term *)

let app_lvl = 2 and abs_lvl = 1 and ini_lvl = 0
let print_term t =
  let rec fn nv b t = match (* whnf *) t with
    App(t1,t2) ->
      if b >= app_lvl then print_string "(";
      fn nv abs_lvl t1;
      print_string " ";
      fn nv app_lvl t2;
      if b >= app_lvl then print_string ")"
  | Abs _ as t ->
      if b >= abs_lvl then print_string "(";
      print_string "\\";
      let rec gn nv = function
	  Abs (s,f) ->
            let name = "x"^(string_of_int nv) in
	    begin 
              match f with bind (fVar s) x as name in t ->
              print_string name;
              print_string " ";
              gn (nv + 1) t
	    end
	| t -> 
            print_string "-> ";
            fn nv ini_lvl t
      in gn nv t; 
      if b >= abs_lvl then print_string ")"
  | Cst c -> 
      print_string c.cst_name
  | UCst (n,_) ->
      print_string "!";
      print_int n
  | UVar{contents = Known t} -> 
      fn nv app_lvl t
  | FVar(x,_) ->     
      print_string (name_of x)
  | _ -> 
      print_string "?"
  in fn 0 ini_lvl t


let alpha = Atom "alpha" and beta = Atom "beta" and gamma = Atom "gamma"
let c1 = Cst {cst_name = "c1"; cst_sort = alpha}
let c2 = Cst {cst_name = "c2"; cst_sort = alpha}
let idta = unbox (Abs(^ (^alpha^),bind (fVar alpha) x in x^) )
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
    Clash -> print_endline "Clash\n"
  | Too_Deep -> print_endline "Too_Deep\n"


let _ = test 10 c1 c1
let _ = test 10 c1 c2
let _ = test 10 (App(idta,c1)) (App(idta,c1))
let _ = test 10 (App(idta,c1)) (App(idta,c2))
let _ = test 10 (App(idta,c1)) (App(new_var(Arrow(alpha,alpha)),c1))
let _ = test 10 (App(idta,c1)) (App(new_var(Arrow(alpha,alpha)),c2))
let _ = test 10 (App(idta,c1)) (App(App(new_var(Arrow(alpha,(Arrow(Arrow(alpha,alpha),alpha)))),c1),idta))

let _ = let v = new_var(Arrow(alpha,alpha)) in 
  test 10 (App(v,c1)) (App(f,App(v,c1)))

let _ = let v = new_var(Arrow(alpha,alpha)) in 
  test 10 (App(v,App(v,c1))) (App(f,App(v,App(v,c1))))

let _ = let v = new_var(Arrow(alpha,Arrow(alpha,alpha))) in 
  test 10 c1 (App(App(v,c1),c2))

let _ = let v = new_var(Arrow(alpha,Arrow(alpha,alpha))) in 
  test 10 c2 (App(App(v,c1),c2))

        

