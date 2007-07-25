open Nvbindlib

type 'a opt_fun = 
    Fun1 of ('a -> 'a)
  | Fun2 of ('a -> 'a -> 'a)
  | Fun3 of ('a -> 'a -> 'a -> 'a)
  | NoOpt

type value =
  | ValUnit
  | ValInt of int
  | ValBool of bool
  | ValFun of (value, value) binder
  | ValVar of value variable

and def_struct =
    { mutable arity : int;
      mutable def_value : value;
      mutable def_fun : value -> value;
      mutable def_repr : Obj.t;
      mutable ready : bool
    }

type ext_struct =
    { ext_value : value;
      ext_repr : Obj.t;
      opt_value : value opt_fun;
      opt_repr : Obj.t opt_fun;
    }

type term =
    Int of int
  | Bool of bool
  | Var of string
  | App of term * term
  | Abs of string * term
  | If of term * term * term
  | LetRec of string * string * term * term 
  | External of ext_struct

let lambda v = ValVar v
let any _ = assert false

let ext_plus = External{ 
  ext_value = ValFun (fun (ValInt n) -> ValFun (fun (ValInt m) -> ValInt (n + m)));
  opt_value = Fun2(fun (ValInt n) (ValInt m) -> ValInt(n+m));
  ext_repr = Obj.repr (+);
  opt_repr = Fun2(Obj.magic (+))}

let ext_moins = External{ 
  ext_value = ValFun (fun (ValInt n) -> ValFun (fun (ValInt m) -> ValInt (n - m)));
  opt_value = Fun2(fun (ValInt n) (ValInt m) -> ValInt(n-m));
  ext_repr = Obj.repr (-);
  opt_repr = Fun2(Obj.magic (-))}

let ext_leq = External{ 
  ext_value = ValFun (fun (ValInt n) -> ValFun (fun (ValInt m) -> ValBool (n <= m)));
  opt_value = Fun2(fun (ValInt n) (ValInt m) -> ValBool(n<=m));
  ext_repr = Obj.repr ( <= );
  opt_repr = Fun2(Obj.magic ( <= ))}
    
let definition = ref []

let app v1 v2 = 
  match v1 with ValFun f -> f v2

let is_def arity env name =
  not (List.mem_assoc name env) &&
  try
    match List.assoc name !definition with
      {arity = a} when a >= arity -> true
    | _ -> false
  with
    Not_found -> false

let compile term =
  let rec fn env = function
      Int i -> (^ValInt i^)
    | Bool b -> (^ValBool b^)
    | Var(name) -> 
	begin try
	  List.assoc name env
	with Not_found -> try
	  let def = List.assoc name !definition in
	  match def with
	    { ready = true; def_value = v } -> 
	      (^v^)
	  | { arity = 0; ready = false } -> 
	      assert false (* no recursive constant allowed *)
	  | _ ->
	      (^ ValFun(fun v -> def.def_fun v) ^)
	with Not_found ->
	  failwith ("unbound "^name)
	end
    | App(Var name,t1) when is_def 1 env name ->
	begin
	  match List.assoc name !definition with
	    {ready = true; def_fun = f}  -> 
	      unit_apply f (fn env t1)
	  | def -> 
	      unit_apply (fun v -> def.def_fun v) (fn env t1)
	end
    | App(App(App(External {opt_value = Fun3(f)},t1),t2),t3) ->
	unit_apply3 f (fn env t1) (fn env t2) (fn env t3)
    | App(App(External {opt_value = Fun2(f)},t1),t2) ->
	unit_apply2 f (fn env t1) (fn env t2)
    | App(External {opt_value = Fun1(f)},t1) ->
	unit_apply f (fn env t1)
    | App(t1, t2) -> 
	unit_apply2 app (fn env t1) (fn env t2)
    | Abs(name, t) ->
	ValFun(^bind lambda x in fn ((name,x)::env) t^)
    | If(b,t1,t2) -> 
	let ft1 = bind any x in  fn env t1 in
	let ft2 = bind any x in  fn env t2 in
	unit_apply3 (fun b t1 t2 -> 
	  if b = ValBool true then t1 () else t2 ()) 
	  (fn env b) ft1 ft2
    | LetRec(name,name2,t1,t2) ->
	let ct1 = fixpoint (bind any r in 
	           let r' = ValFun(^r^) in 
	           bind lambda x in fn ((name,r')::(name2,x)::env) t1) in
        let ct2 = bind lambda r in fn ((name,r)::env) t2 in       
	bind_apply ct2 (ValFun(^ct1^) )
    | External v -> (^v.ext_value^)
  in
  unbox (fn [] term)

let compile_unsafe term =
  let rec fn env = function
      Int i -> (^Obj.repr i^)
    | Bool b ->	(^Obj.repr b^)
    | Var(name) -> 
	begin try
	  List.assoc name env 
	with Not_found -> try
	  let def = List.assoc name !definition in
	  match def with
	    { ready = true; def_repr = v } -> 
	      (^v^)
	  | { arity = 0; ready = false } -> 
	      assert false (* no recursive constant allowed *)
	  | _ ->
	      (^ Obj.repr (fun v -> Obj.obj def.def_repr v) ^)
	with Not_found ->
	  failwith ("unbound "^name)
	end
    | App(Var name,t1) when is_def 1 env name ->
	begin
	  match List.assoc name !definition with
	    {ready = true; def_repr = f}  -> 
	      unit_apply (Obj.obj f) (fn env t1)
	  | def -> 
	      unit_apply (fun v -> Obj.obj def.def_repr v) (fn env t1)
	end
    | App(App(App(External {opt_repr = Fun3(f)},t1),t2),t3) ->
	unit_apply3 f (fn env t1) (fn env t2) (fn env t3)
    | App(App(External {opt_repr = Fun2(f)},t1),t2) ->
	unit_apply2 f (fn env t1) (fn env t2)
    | App(External {opt_repr = Fun1(f)},t1) ->
	unit_apply f (fn env t1)
    | App(t1, t2) -> 
	apply (Obj.magic (fn env t1)) (fn env t2)
    | Abs(name, t) ->
	Obj.magic (bind any x in fn ((name,x)::env) t)
    | If(b,t1,t2) -> 
	let ft1 = bind any x in  fn env t1 in
	let ft2 = bind any x in  fn env t2 in
	unit_apply3 (fun b t1 t2 -> 
	  if Obj.obj b then t1 () else t2 ()) 
	  (fn env b) ft1 ft2
    | LetRec(name,name2,t1,t2) ->
	let ct1 = fixpoint (bind any r in 
	           bind any x in 
                    fn ((name,Obj.magic r)::(name2,x)::env) t1) in
        let ct2 = bind any r in fn ((name,r)::env) t2 in       
	bind_apply (Obj.magic ct2) ct1
    | External v -> 
	(^v.ext_repr^)
  in
  unbox (fn [] term)

let make_definition name term =
  let rec count_abs = function
      Abs(name,t) -> 1 + count_abs t
    | _ -> 0
  in
  let arity = count_abs term in
  let def = {arity = arity;
	     ready=false; def_value = ValUnit; def_repr = Obj.repr (); 
	     def_fun = fun _ -> assert false} in
  definition := (name,def)::!definition;
  let v = compile term in
  let vo = compile_unsafe term in
  def.ready <- true;
  def.def_value <- v;
  def.def_repr <- vo;
  begin
    match v with
      ValFun(f) -> def.def_fun <- f; def.arity <- max 1 arity
  end; 
  v, vo


let fib =
  Abs("n",
	 If(App(App(ext_leq, Var "n"), Int 1),
		    Int 1,
		    App(App(ext_plus,App(Var "fib",App(App(ext_moins,Var "n"), Int 1))),
			  App(Var "fib",App(App(ext_moins,Var "n"), Int 2)))))

let cfib, ofib = make_definition "fib" fib

let _ = print_string "*"; print_newline ()


let _ = match app cfib (ValInt 31) with
  ValInt n -> print_int n; print_newline ()
| _ -> assert false

(*
let _ = print_int (Obj.magic ofib 31); print_newline ()
*)
