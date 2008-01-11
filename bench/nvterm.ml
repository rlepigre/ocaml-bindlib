(* lets open the library ! *)
open Nvbindlib

(* type of pure lambda-terms *)

type term =
  App of term * term
| Abs of (term,term) binder 

| FVar of term variable (* constructor used for normalisation only *)

let lambda (x:term variable) = FVar(x)

(* printing of term *)

let app_lvl = 2
let abs_lvl = 1
let ini_lvl = 0

let print_term t =
  let rec fn nv b = function
      App(t1,t2) ->
      	if b >= app_lvl then print_string "(";
      	fn nv abs_lvl t1;
      	print_string " ";
      	fn nv app_lvl t2;
      	if b >= app_lvl then print_string ")"
    | Abs _ as t ->
    	if b >= abs_lvl then print_string "(";
    	print_string "fun ";
        let rec gn nv = function
	    Abs f ->
	      (match f with bind lambda x in f' ->
		print_string "x";
		print_string " ";
		gn (nv + 1) f')
	  | t -> 
              print_string "-> ";
              fn nv ini_lvl t
    	in gn nv t;
    	if b >= abs_lvl then print_string ")"
    | FVar(v) ->
    	print_string "x"
  in fn 0 ini_lvl t; print_newline ()

(* weak head normal form *)

let rec whnf = function
  App(t1,t2) as t0 -> (
    match (whnf t1) with
      Abs f -> whnf (subst f t2)
    | t1' -> if t1' != t1 then App(t1', t2) else t0)
| t -> t

(* call by name normalisation *)
let norm t = let rec fn t = 
  match whnf t with
    Abs f -> 
      Abs(^ bind lambda x in fn (subst f (unbox x)) ^)
  | t -> 
      let rec unwind = function
	  FVar(x) -> bindbox_of x
	| App(t1,t2) -> App(^unwind t1,fn t2^)
	| t -> assert false
      in unwind t
in unbox (fn t)

(* another call by name normalisation *)
(* this time not using whnf, but using a stack *)
let norm' t = 
  let rec fn stack = function
      App(t1,t2) -> fn (t2::stack) t1
    | Abs f -> (match stack with
	[] -> 
	  Abs(^ bind lambda x in fn [] (subst f (unbox x)) ^)
	| t::stack -> fn stack (subst f t))
    | FVar(x) -> 
	List.fold_left (fun t u -> App(^t,u^) ) (bindbox_of x) (List.map (fn []) stack)
  in unbox (fn [] t)




(* right normalisation *)

let norm_right t =
  let rec fn = function
      App(t1, t2) ->
    	let t2' = fn t2 in
    	let t1' = fn t1 in
    	begin
	  match unbox t1' with
     	    Abs f -> 
	      fn (subst f (unbox t2'))
	  | _ -> App(^t1', t2'^)
    	end
    | Abs f ->
	Abs(^ bind lambda x in fn (subst f (unbox x)) ^)
    | FVar(x) ->
    	bindbox_of x
  in unbox (fn t)

let mark t =
  let rec phi x = function
    FVar(y) -> bindbox_of y
  | App(u,v) -> App(^App(^x,phi x u^),phi x v^)
  | Abs(f) ->
     Abs(^ bind lambda y in phi x (subst f (unbox y)) ^)
  in
  unbox (Abs(^ bind lambda x in phi x t^) )

(* examples of terms *)
let idt = unbox (Abs(^ bind lambda x in x^) )

let delta = unbox (Abs(^ bind lambda x in App(^x,x^) ^) )

let _ = print_string "delta'"

let delta' = unbox (Abs(^ bind lambda x in Abs(^ bind lambda z in Abs(^ bind lambda y in App(^z,App(^x,y^) ^) ^) ^) ^) )

let _ = print_newline ()

let zero = 
  unbox (Abs(^ bind lambda f in Abs(^ bind lambda x in x ^) ^) )

let tfalse = zero

let ttrue = 
  unbox (Abs(^ bind lambda f in Abs(^ bind lambda x in f ^) ^) )

let _ =
    let print_bool b =
      print_string (if b then "true" else "false") in
    let is_binder_closed = function
      Abs f ->  is_binder_closed f
    | _ -> assert false in	  
    print_string "check is_binder_closed";
    print_newline ();
    print_term idt; print_string " : "; 
    print_bool (is_binder_closed idt); print_newline ();
    print_term tfalse; print_string " : "; 
    print_bool (is_binder_closed tfalse); print_newline ();
    print_term ttrue; print_string " : "; 
    print_bool (is_binder_closed ttrue); print_newline ();
    print_term (whnf (App(ttrue,idt))); print_string " : "; 
    print_bool (is_binder_closed  (whnf (App(ttrue,idt))));
    print_newline ()

let succ = 
  unbox (Abs(^ bind lambda n in Abs(^ bind lambda f in 
              Abs(^ bind lambda x in 
                App(^f,App(^App(^n,f^),x ^) ^) ^) ^) ^) )
let succ' = 
  unbox (Abs(^ bind lambda n in Abs(^ bind lambda f in 
              Abs(^ bind lambda x in 
		App(^App(^n,f^),App(^f,x^) ^) ^) ^) ^) )

let two = App(succ,App(succ,zero))
let four = App(two,two) 

let _ =
  print_term (norm_right four); print_newline ()

let _ =
  print_term (mark delta); print_newline ()

let _ =
  print_term delta'; print_newline ()

let plus =   unbox (Abs(^ bind lambda n in Abs(^ bind lambda m in 
               Abs(^ bind lambda f in 
               Abs(^ bind lambda x in App(^App(^n, f^),App(^App(^m, f^), x^) ^) ^) ^) ^) ^) )

let mul =   unbox (Abs(^ bind lambda n in Abs(^ bind lambda m in 
               Abs(^ bind lambda f in 
                App(^n,App(^m,f^) ^) ^) ^) ^) ) 

let height = App(App(plus,four) ,four) 
let ten = App(App(plus,two) ,height) 
let hundred = App(App(mul,ten) ,ten) 
let thousand = App(App(mul,hundred) ,ten) 


let pred = unbox (Abs(^ bind lambda n in
  App(^App(^App(^App(^n,
    Abs(^ bind lambda p in Abs(^ bind lambda x in Abs(^ bind lambda y in
       App(^App(^p,App(^ (^ succ ^) ,x ^) ^), x^) ^) ^) ^) ^),
    Abs(^ bind lambda x in Abs(^ bind lambda y in y^) ^) ^) ,
    (^ zero ^) ^), (^ zero ^) ^) ^) )


let bench () =
  let fh = App(App(mul,four),hundred) in  
  print_term (norm' (App(App(fh,pred),fh)));
  print_newline();
  print_term (norm' (App(App(mul,ten),thousand)));
  print_newline()
;;

bench ()
;;