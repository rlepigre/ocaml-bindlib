(**
Bindlib library
@author Christophe Raffalli
*)

(** the environment type: it is used to store the value of all bound 
   variables. We need the "Obj" module because all the bound variables
   may have diffrent types and we want to store them in one array *)

type environment = Obj.t

let create_env size = Obj.new_block 0 size
let set_env env i x = Obj.set_field env i (Obj.repr x)
let get_env env i   = Obj.obj (Obj.field env i)


(* Context:  We need map of strings : *)
module String_ord = struct 
  type t = string
  let compare = compare
end
module SMap = Map.Make(String_ord)

type context = int list SMap.t

let empty_context = SMap.empty

let get_suffix name suffix ctxt =
  try
    let l = SMap.find name ctxt in
    let rec search acc suffix = function
	[] -> suffix, List.rev_append acc [suffix]
      | x::_ as l when x > suffix -> suffix, List.rev_append acc (suffix::l)
      | x::l when x = suffix -> search (x::acc) (suffix+1) l
      | x::l (* when x < suffix *) -> search (x::acc) suffix l
    in
    let suffix, l = search [] suffix l in
    let ctxt = SMap.add name l ctxt in
    suffix, ctxt
  with
    Not_found ->
      let ctxt = SMap.add name [suffix] ctxt in
      suffix, ctxt

(* We need int map and we use J.-C. Filliatre's implementation of Patricia trees *)

module IntMap = Ptmap
let get_suffix2 collision htbl suffix =
  let l = List.map (fun key -> snd (IntMap.find key htbl)) collision in
  let l = List.sort (-) l in
  let rec search suffix = function
	[] -> suffix
    | x::l when x > suffix -> suffix
    | x::l when x = suffix -> search (suffix+1) l
    | x::l (* when x < suffix *) -> search suffix l
  in
  search suffix l


(* The type parpos will be use to associate to each variables (identified
by a unique integer key) its position in the environment *)

type varpos = (int * int) IntMap.t

(* object with bound variables will be encoded using functions waiting 
for two arguments:
  - a map of type "varpos" giving the position of variables in the environement
  - an environment.

An important remark: the function will use the first argument to
produced efficient substitution, producing a closure waiting the
environment. This means that the map of type varpos is used only once
for each variable even if the term is used many times 
*)

type 'a env_term = varpos -> environment -> 'a

type any = Obj.t 

type 'a bindbox = 

    (* the term as no free variables *)
    Closed of 'a 

    (* the general case : 
     In Open(vl,nb,t), 
     - vl is the list of all free variables sorted by key.
       we have to use "any" because a 'a bindbox may use variables of
       any type. This could be implemented using existential type ...
     - nb is the number of bound variables which must have a place reserved
       in the environment.
     - t is the open_term itself. The map that t waits as first argument must
       contain the position in the environment of all variables in vl.
     *)
     	
  | Open of any variable list * int * 'a env_term

and 'a variable = 
  { key : int;                          (* a unique identifier for each variable *)
    var_name : string;                  (* the name as a free variables*)
    prefix : string;                    (* the prefix to avoid to compute it *)
    suffix : int;
    
    mkfree : 'a variable -> 'a; 
    mutable bindbox : 'a bindbox }

(** Variables managment *)

let merge_name prefix suffix =
  if suffix >= 0 then prefix^(string_of_int suffix) else prefix

let name_of var = var.var_name

(** split name : split a string into a string and an int which is the suffix 
    of the variable or -1 if the string ends with no digit *)
let split_name name =
  let n = String.length name in
  let p = ref (n-1) in
  while !p >= 0 && '0' <= name.[!p] &&  name.[!p] <= '9' do
    decr p
  done;
  let p = !p + 1 in
  if p = n or p = 0 then name, (-1) else
  String.sub name 0 p, int_of_string (String.sub name p (n - p))

let compare_variables v1 v2 =
  Pervasives.compare v1.key v2.key

let get_var_key v = v.key

(** merge l l' merge two sorted lists in one sorted list without repetition *)
let rec merge l l' = 
  match l, l' with
    [], l -> l
  | l, [] -> l
  | ({key = x} as c::ls as l),({key = x'} as c'::ls' as l') ->
    if x = x' then merge ls l'
    else if x <= x' then c::(merge ls l')
    else c'::(merge l ls')

let search var l =
  let k = var.key in
  let rec fn acc = function 
      [] -> raise Not_found
    | v::l when v.key > k -> raise Not_found
    | v::l when v.key = k -> List.rev_append acc l
    | v::l -> fn (v::acc) l
  in 
  fn [] l

(** the type of binder: ('a,'b) binder is an expression of type 'b 
    with a bound variable of type 'a. It is exported abstract *)
type ('a,'b) binder = string * ('a -> 'b)
    
(** the substitution, it is just an application ! *)
let subst f x = snd f x

(** get the name of the binder *)
let binder_name f = fst f

(** the type of multiple binder (binding n variables at once): 
('a,'b) mbinder is an expression of type 'b 
    with a n bound variables of type 'a. It is exported abstract *)
type ('a,'b) mbinder = string array * ('a array -> 'b)
    
(** the substitution, it is just an application ! *)
let msubst f x = snd f x

(** get the names of the binder *)
let mbinder_names f = fst f
let binder_names = mbinder_names

(** get the arity of the binder *)
let mbinder_arity f = Array.length (fst f)
let binder_arity = mbinder_arity

(** it is sometimes nice to have a "dummy" bindbox. This avoid extra type
constructor is some circonstances and may play the role of "None" *)

let dummy_bindbox =
  Open([], 0, fun _ -> failwith "Invalid use of dummy_bindbox")

(* the function that creates the number of a new variable *)
let count = ref 0

let reset_bindlib_count () =
  count := 0

let mk_var index v = get_env v index

let mk_var2 var tbl =
  let index = fst (IntMap.find var.key tbl) in mk_var index

let generalise_var = ((fun var -> Obj.magic var) : 'a variable -> any variable)

let new_var (bv  : 'a variable -> 'a) name =
  incr count;
  if !count < 0 then failwith "variable loop (buy a 64 bits)";
  let prefix, suffix = split_name name in
  let rec var = { 
    key = !count; 
    var_name = name;
    prefix = prefix;
    suffix = suffix;
    mkfree = bv; 
    bindbox = dummy_bindbox }
  in
  let result = Open([generalise_var var], 0, mk_var2 var) in
  var.bindbox <- result;
  var

let new_var_in ctxt (bv  : 'a variable -> 'a) name =
  incr count;
  if !count < 0 then failwith "variable loop (buy a 64 bits)";
  let prefix, suffix = split_name name in
  let new_suffix, ctxt = get_suffix prefix suffix ctxt in
  let rec var = { 
    key = !count; 
    var_name = merge_name prefix new_suffix;
    prefix = prefix;
    suffix = suffix;
    mkfree = bv; 
    bindbox = dummy_bindbox }
  in
  let result = Open([generalise_var var], 0, mk_var2 var) in
  var.bindbox <- result;
  var, ctxt

let bindbox_of v = v.bindbox
let free_of v = v.mkfree v

(* Construct a 'a bindbox to represent the variable numbered var 
   You can notice that we use the environment to store values as a hashtable 
   with no failure (we always find the value the first time we look for it)
   The last case in the environment is used to store the position of the next
   variable we can assign in the environment.
*)


(* take a term of type 'a and turn it into a 'a bindbox that can be used to
    construct a larger term with binder *)
let unit t = Closed(t)

(* take a function t and an environment v and create an environment nv
   with places for future values of bound variables that t needs. 

   table.(i) gives the position in v of the variable with index i in nv
   table.(0) is unused.
   esize is the total size of nv
   nv.(0) is filled with the first unitialized position in nv which is
   Array.length table.
*)
let mk_select t esize table v =
  let nsize = Array.length table in
  let nv = create_env esize in
  set_env nv 0 nsize;
  for i = 1 to nsize - 1 do
    set_env nv i (get_env v table.(i))
  done;
  t nv

let mk_select2 t nbbound frees uptbl =
  let table = Array.create (List.length frees + 1) 0 in
  let cur = ref 0 in
  let downtbl = List.fold_left (fun htbl var ->
    incr cur;
    let upindex, suffix = IntMap.find var.key uptbl in
    table.(!cur) <- upindex;
    IntMap.add var.key (!cur,suffix) htbl) IntMap.empty frees
  in
  let size = !cur + nbbound + 1 in
  mk_select (t downtbl) size table

let select frees nbbound t = 
  if nbbound = 0 then
    t
  else
    mk_select2 t nbbound frees

let mk_apply f a v = f v (a v)
let mk_lapply f a v = f (a v)
let mk_rapply f a v = f v a

let mk_apply2 f a h = mk_apply (f h) (a h)
let mk_lapply2 f a h = mk_lapply f (a h)
let mk_rapply2 f a h = mk_rapply (f h) a

(** the "apply" function of the monad: take an object of type ('a ->
  'b) bindbox, that is a function with some free variables, an
  argument of type 'a bindbox, and build the application, of type 'b
  bindbox which is a term that may also have free variables. The
  function select is used here to build the "minimal" closure when both
  the function and the argument have free variables *)

let apply tf ta =
  match tf, ta with
    Closed(f), Closed (a) -> Closed (f a)
  | Closed(f), Open(va,ba,a) -> 
      Open(va,ba,mk_lapply2 f a)
  | Open(vf,bf,f), Closed(a) -> 
      Open(vf, bf, mk_rapply2 f a)
  | Open(vf,nbf,f), Open(va,nba,a) ->
      let f = select vf nbf f in
      let a = select va nba a in
      let vars = merge vf va in
      Open(vars, 0, mk_apply2 f a)

(* used for a binder when you bind a variable in closed term (therefore that *)
(* variable does not occur in the term ! *)
let mk_closed_bind pt = 
  fun _ -> pt

let head_mk_closed_bind =
  let f = mk_closed_bind () in
  Obj.field (Obj.repr f) 0

(* used for binder which binds a variable with no occurrence (but the term has*)
(* other free variables *)
let mk_mute_bind name pt v = name, fun _ ->
  pt v

let mk_mute_bind2 collision prefix suffix pt htbl = 
  let suffix = get_suffix2 collision htbl suffix in
  let name = merge_name prefix suffix in
  mk_mute_bind name (pt htbl)

let head_mk_mute_bind =
  let f = snd (mk_mute_bind "" (fun x y -> y x) ()) in
  Obj.field (Obj.repr f) 0

(* check if the variable bound in (f : ('a,'b) binder) occurs *)
let is_binder_constant f =
  let f = Obj.repr (snd f) in
  let tag = Obj.tag f in
  assert (tag = Obj.closure_tag);
  let head = Obj.field f 0 in
  (head == head_mk_closed_bind) or (head == head_mk_mute_bind)

(* check if a binder is a closed term *)
let is_binder_closed f =
  let f = Obj.repr (snd f) in
  let tag = Obj.tag f in
  assert (tag = Obj.closure_tag);
  let head = Obj.field f 0 in
  (head == head_mk_closed_bind)

let is_mbinder_constant = is_binder_constant
let is_mbinder_closed = is_binder_closed

(* used for the first binder in a closed term (the binder that binds the last*)
(* free variable in a term and make it a closed term *)
let mk_first_bind esize pt arg =
  let v = create_env esize in
  set_env v 0 2;
  set_env v 1 arg;
  pt v

let mk_first_bind2 prefix suffix key esize pt = 
  let htbl = IntMap.empty in
  let htbl = IntMap.add key (1,suffix) htbl in
  merge_name prefix suffix, mk_first_bind esize (pt htbl)

(* used for the general case of a binder *)
let mk_bind name pos pt v = name, fun arg ->
  let next = get_env v 0 in 
  if next = pos then begin
    set_env v 0 (next + 1);
    set_env v next arg;
    pt v
  end else begin
    let v = Obj.dup v in 
    set_env v 0 (pos + 1);
    set_env v pos arg;
    for i = pos + 1 to next - 1 do
      set_env v i 0
    done;
    pt v
  end

let mk_bind2 collision prefix suffix key pos pt htbl =
  let suffix = get_suffix2 collision htbl suffix in
  let name = merge_name prefix suffix in
  let htbl = IntMap.add key (pos, suffix) htbl in
  mk_bind name pos (pt htbl)

let filter_map cond fn l =
  let rec gn acc = function
      [] -> List.rev acc
    | x::l -> if cond x then gn (fn x::acc) l else gn acc l
  in gn [] l

let bind_aux v t = match t with 
     Closed(t) ->
       Closed (v.var_name, mk_closed_bind t)
   | Open(vt,nbt,t) -> 
       try 
         match vt with
          [var] -> 
            if v.key <> var.key then raise Not_found;
            let esize = nbt + 2 in
            Closed (mk_first_bind2 v.prefix v.suffix v.key esize t)
        | _ ->
            let vt = search v vt in
	    let collision = filter_map (fun v' -> v.prefix = v'.prefix) 
		(fun v -> v.key) vt in
	    let pos = List.length vt + 1 in
            Open(vt, nbt+1, mk_bind2 collision v.prefix v.suffix v.key pos t)
      with Not_found ->
	let collision = filter_map (fun v' -> v.prefix = v'.prefix) 
	    (fun v -> v.key) vt in
	Open(vt, nbt, mk_mute_bind2 collision v.prefix v.suffix t)

(* take a function of type ('a bindbox -> 'b bindbox) and transform it into a binder*) 
(* of type ('a, 'b) binder bindbox *)
let bind bv name fpt =
  let v = new_var bv name in
  bind_aux v (fpt v.bindbox)

let bind_in ctxt bv name fpt =
  let v, ctxt = new_var_in ctxt bv name in
  bind_aux v (fpt v.bindbox ctxt)

exception Bindlib_Not_Variable

let bind_var = bind_aux

let mk_mbind names pos access pt v = 
  names, fun args ->
    let arity = Array.length names in
    let size = Array.length args in
    if size <> arity then raise (Invalid_argument "bad arity in msubst");
    let next = get_env v 0 in
    let cur_pos = ref pos in
    if next = pos then begin
      for i = 0 to arity - 1 do
	if access.(i) then begin
	  set_env v !cur_pos args.(i);
	  incr cur_pos;
	end
      done;
      set_env v 0 !cur_pos;
      pt v
    end else begin
      let v = Obj.dup v in 
      for i = 0 to arity - 1 do
	if access.(i) then begin
	  set_env v !cur_pos args.(i);
	  incr cur_pos;
	end
      done;
      set_env v 0 !cur_pos;
      for i = !cur_pos to next - 1 do
	set_env v i 0
      done;
      pt v
    end

let mk_mbind2 colls prefixes suffixes keys pos pt htbl =
  let cur_pos = ref pos in
  let htbl = ref htbl in
  let new_names = Array.create (Array.length prefixes) "" in
  let access = Array.mapi (fun i key ->
    let suffix = get_suffix2 colls.(i) !htbl suffixes.(i) in
    new_names.(i) <- merge_name prefixes.(i) suffix;
    if key <> 0 then begin
      htbl := IntMap.add key (!cur_pos,suffix) !htbl;
      incr cur_pos;
      true
    end else
      false
      ) keys
  in
  mk_mbind new_names pos access (pt !htbl)


let mk_closed_mbind names pt v = 
  names, fun args ->
    let arity = Array.length names in
    let size = Array.length args in
    if size <> arity then raise (Invalid_argument "bad arity in msubst");
    pt v

let mk_closed_mbind2 colls prefixes suffixes pt htbl =
  let new_names = Array.create (Array.length prefixes) "" in
  Array.iteri (fun i c ->
    let suffix = get_suffix2 c htbl suffixes.(i) in
    new_names.(i) <- merge_name prefixes.(i) suffix;
	      ) colls;
  mk_closed_mbind new_names (pt htbl)

let mk_first_mbind names size access pt = names, fun args ->
  let v = create_env size in
  let arity = Array.length names in
  let size = Array.length args in
  if size <> arity then raise (Invalid_argument "bad arity in msubst");
  let cur_pos = ref 1 in
  for i = 0 to arity - 1 do
    if access.(i) then begin
      set_env v !cur_pos args.(i);
      incr cur_pos;
    end
  done;
  set_env v 0 !cur_pos;
  pt v

let mk_first_mbind2 colls prefixes suffixes keys size pt =
  let cur_pos = ref 1 in
  let htbl = ref IntMap.empty in
  let new_names = Array.create (Array.length prefixes) "" in
  let access = Array.mapi (fun i key ->
    let suffix = get_suffix2 colls.(i) !htbl suffixes.(i) in
    new_names.(i) <- merge_name prefixes.(i) suffix;
    if key <> 0 then begin
      htbl := IntMap.add key (!cur_pos,suffix) !htbl;
      incr cur_pos;
      true
    end else
      false
      ) keys
  in
  mk_first_mbind new_names size access (pt !htbl)

let mbind_aux vs t = match t with 
     Closed(t) ->
       let names = Array.map (fun v -> v.var_name) vs in
       Closed (names, mk_closed_bind t)
   | Open(vt,nbt,t) -> 
       let vt = ref vt in
       let nnbt = ref nbt in
       let len = Array.length vs in
       let prefixes = Array.create len "" in 
       let suffixes = Array.create len (-1) in 
       let colls = Array.create len [] in 
       let keys = Array.create len 0 in 
       for i = len - 1 downto 0 do
	 let v = vs.(i) in
	 prefixes.(i) <- v.prefix;
	 suffixes.(i) <- v.suffix;
	 try
	   let ng = search v !vt in
	   colls.(i) <- filter_map (fun v' -> v.prefix = v'.prefix) 
	       (fun v -> v.key) ng;
	   incr nnbt;
	   vt := ng;
	   keys.(i) <- v.key
	 with Not_found ->
	   colls.(i) <- filter_map (fun v' -> v.prefix = v'.prefix) 
	       (fun v -> v.key) !vt;
	   keys.(i) <- 0
       done;
       if !vt = [] then
	 Closed(mk_first_mbind2 colls prefixes suffixes keys (!nnbt + 1) t)
       else if !nnbt = nbt then
	 Open(!vt,nbt,mk_closed_mbind2 colls prefixes suffixes t)
       else
	 let pos = List.length !vt + 1 in
	 Open(!vt,!nnbt,mk_mbind2 colls prefixes suffixes keys pos t)

(* take a function of type ('a bindbox array -> 'b bindbox) and transform it into a binder*) 
(* of type ('a, 'b) mbinder open_term *)

let new_mvar bv names =
  Array.map (fun name -> new_var bv name) names

let new_mvar_in ctxt bv names =
  let ctxt = ref ctxt in
  let vs = Array.map 
    (fun name -> 
      let v, new_ctxt = new_var_in !ctxt bv name in
      ctxt := new_ctxt;
      v
    ) names
  in
  vs, !ctxt

type 'a mvariable = 'a variable array

let mbind bv names fpt =
  let vs = new_mvar bv names in
  let args = Array.map bindbox_of vs in
  mbind_aux vs (fpt args)

let mbind_in ctxt bv names fpt =
  let vs,ctxt = new_mvar_in ctxt bv names in
  let args = Array.map bindbox_of vs in
  mbind_aux vs (fpt args ctxt)

let bind_mvar = mbind_aux

(* When a term has no free variable, you can get it ! *)
let unbox t = 
  let fn (t : 'a bindbox) = match t with 
    Closed(t) ->
      t
  | Open(vt,nbt,t) -> 
      let next =  List.length vt + 1 in
      let esize = next + nbt in
      let env = create_env esize in
      set_env env 0 next;
      let cur = ref 0 in
      let htbl = List.fold_left (fun htbl var ->
	incr cur;
	set_env env !cur (var.mkfree var);
	let _, suffix = split_name var.var_name in
	IntMap.add var.key (!cur, suffix) htbl
	) IntMap.empty vt
      in
      t htbl env
  in fn t

let is_closed = function
   Closed(_) -> true
 | _ -> false

(* Here are some usefull function *)
(* Some of them are optimised (the comment is the simple definition) *)

(*
let unit_apply f ta = apply (unit f) ta
*)
let unit_apply f ta =
  match ta with
    Closed(a) -> Closed (f a)
  | Open(va,ba,a) -> 
      Open(va, ba, mk_lapply2 f a)

let mk_uapply f a b v = f (a v) (b v)
let mk_luapply f a b v = f a (b v)
let mk_ruapply f a b v = f (a v) b

let mk_uapply2 f a b h = mk_uapply f (a h) (b h)
let mk_luapply2 f a b h = mk_luapply f a (b h)
let mk_ruapply2 f a b h = mk_ruapply f (a h) b

let unit_apply2 f ta tb =
  match ta, tb with
    Closed(a), Closed (b) -> Closed (f a b)
  | Closed(a), Open(vb,bb,b) -> 
      Open(vb,bb,mk_luapply2 f a b)
  | Open(va,ba,a), Closed(b) -> 
      Open(va, ba, mk_ruapply2 f a b)
  | Open(va,ba,a), Open(vb,bb,b) ->
      let a = select va ba a in
      let b = select vb bb b in
      let vars = merge va vb in
      Open(vars, 0, mk_uapply2 f a b)

let unit_apply3 f t t' t'' = apply (unit_apply2 f t t') t''

(* Used in some cases ! *)
let bind_apply f = unit_apply2 snd f

let mbind_apply f = unit_apply2 snd f

let rec cfix t = t (cfix t)

let rec fix t env =  snd (t env) (fix t env)

let fix2 t htbl = fix (t htbl)

let fixpoint (f : (('a, 'b) binder, ('a, 'b) binder) binder bindbox) = 
  (match f with
    Closed t -> Closed(cfix (snd t))
  | Open(vt,nbt,t) -> Open(vt,nbt,fix2 t) : ('a, 'b) binder bindbox)

(* dirty imperative functions ... *)

let mk_unit a _ _ = a 
	
let special_apply tf ta =
  match tf, ta with
    Closed(()), Closed (a) -> Closed (()), mk_unit a
  | Closed(()), Open(va,ba,a) -> 
      Open(va,ba,fun _ _ -> ()), select va ba a
  | Open(vf,nbf,_) as c, Closed(a) -> 
      c, mk_unit a
  | Open(vf,nbf,_), Open(va,ba,a) ->
      let vars = merge vf va in
      Open(vars, 0, fun _ _ -> ()), select va ba a
   
let special_start = Closed ()

let special_end e f = match e with
  Closed _ -> Closed(f IntMap.empty (create_env 0))
| Open(v,b,_) -> Open(v,b,f)

(* to get very nice and efficient functor !*)

module type Map = 
  sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

module Lift(M: Map) =
  struct
    let f t =
      let acc = ref special_start in
      let fn o =
	let nacc, o = special_apply !acc o in
	acc:= nacc;
	o
      in
      let t = M.map fn t in
      special_end !acc (fun htbl -> let l = M.map (fun o -> o htbl) t in
         (fun env -> M.map (fun o -> o env) l))
  end

module type Map2 = 
  sig
    type ('a, 'b) t
    val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
  end

module Lift2(M: Map2) =
  struct
    let f t =
      let acc = ref special_start in
      let fn o =
	let nacc, o = special_apply !acc o in
	acc:=nacc;
	o
      in
      let t = M.map fn fn t in
      special_end !acc (fun htbl -> let l = M.map (fun o -> o htbl) (fun o -> o htbl) t in
         (fun env -> M.map (fun o -> o env) (fun o -> o env) l))
  end


module Map_list =
  struct 
    type 'a t = 'a list
    let map = List.rev_map (* since we map twice ... this is better ! *)
  end

module Lift_list = Lift(Map_list)

let lift_list = Lift_list.f

module Map_array =
  struct 
    type 'a t = 'a array
    let map = Array.map (* since we map twice ... this is better ! *)
  end

module Lift_array = Lift(Map_array)

let lift_array = Lift_array.f

let lift_pair x y = unit_apply2 (fun x y -> x,y) x y

let copy_var var name mkfree = 
  { var with var_name = name; mkfree = mkfree }
