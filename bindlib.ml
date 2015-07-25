(****************************************************************************
 * The Bindlib Library provides datatypes to represent binders in arbitrary *
 * languages. The implementation is efficient and manages name in the expe- *
 * ted way (variables have a prefered name to which an integer  suffix  can *
 * be added to avoid capture during substitution.                           *
 *                                                                          *
 * Author: Christophe Raffalli                                              *
 * Modified by: Rodolphe Lepigre                                            *
 ****************************************************************************)

open Util

(* In the internals of bindlib, each variable is identified by a unique (int)
key. Closures are then be formed by mapping free variables in an environment
represented using an array. The type [varpos] provides hashtables associating
each variable a couple of its index in the environment and the integer suffix
of its name (used for renaming in capture-avoiding substitution). *)
type varpos = (int * int) IMap.t

(* Type of a term of type ['a] containing free variables. *)
type 'a bindbox =
  (* Closed term (i.e. containing no free variable). *)
  | Closed of 'a
  (* Open term (general case). In [Open(vs,nb,t)] we store:
    - [vs]: the list of every free variables sorted by key. The type [any] is
      used since ['a bindbox] may contain variables of any type.
    - [nb]: the number of bound variables which must have a reserved space in
      then environment.
    - [t]: the open term itself. As a first argument, it should be provided
      with the position of every variables of [vs] in the environment that is
      given as its second argument.
  An important remark: the function will use the first argument to produce
  efficient substitution, producing a closure waiting for an environment.
  This means that the map of type varpos is used only once for each variable
  even if the term is used many times. *)
  | Open of any variable list * int * (varpos -> Env.t -> 'a)

(* Type of a free variable of type ['a]. *)
and 'a variable =
  { key             : int     (* Unique identifier. *)
  ; var_name        : string  (* Name as a free variable. *)
  ; prefix          : string  (* Prefix, i.e. name with no integer suffix. *)
  ; suffix          : int     (* Suffix, i.e. second part of the name. *)
  ; mkfree          : 'a variable -> 'a (* Function to build a term. *)
  ; mutable bindbox : 'a bindbox } (* Bindbox containing the variable. *)

(* Obtain the name of a the given variable. *)
let name_of : 'a variable -> string =
  fun x -> x.var_name

(* Safe comparison function for variables. *)
let compare_variables : 'a variable -> 'a variable -> int =
  fun x y -> Pervasives.compare x.key y.key

(* Hash function for variables. *)
let hash_var : 'a variable -> int =
  fun x -> Hashtbl.hash (`HVar, x.key)

(* Build a term with a free variable from a variable. *)
let free_of : 'a variable -> 'a =
  fun x -> x.mkfree x

(* Build a bindbox from a variable. *)
let box_of_var : 'a variable -> 'a bindbox =
  fun x -> x.bindbox

(* Make a copy of a variable with a potentially different name and syntactic
wrapper but with the same key. *)
let copy_var : 'a variable -> string -> ('a variable -> 'a) -> 'a variable =
  fun x name mkfree -> { x with var_name = name; mkfree = mkfree }

(* Type of multi-variables of type ['a]. *)
type 'a mvariable = 'a variable array

(* Type of a binder, i.e. an expression of type ['b] with a bound variable of
type ['a]. *)
type ('a,'b) binder =
  { name  : string     (* Name of the bound variable. *)
  ; bind  : bool       (* Does the variable occur? *)
  ; rank  : int        (* Number of remaining free variables (>= 0). *)
  ; value : 'a -> 'b } (* Substitution function. *)
    
(* Obtain the name of the bound variable. *)
let binder_name : ('a,'b) binder -> string =
  fun b -> b.name

(* Substitution function (simply an application!). *)
let subst : ('a,'b) binder -> 'a -> 'b =
  fun b x -> b.value x

(* Does the bound variable occur in the binder? *)
let binder_occur : ('a,'b) binder -> bool =
  fun b -> b.bind

(* Is the binder constant? *)
let binder_constant : ('a,'b) binder -> bool =
  fun b -> not b.bind

(* Is the binder a closed term? *)
let binder_closed : ('a,'b) binder -> bool =
  fun b -> b.rank = 0

(* How many free variables does the binder contain? *)
let binder_rank : ('a,'b) binder -> int =
  fun b -> b.rank

(* Type of a multi-binder (binding n variables at once). It is an expression
of type ['b] with n bound variables of type ['a]. *)
type ('a,'b) mbinder =
  { names  : string array     (* Names of the bound variables. *)
  ; binds  : bool array       (* Do the variables occur? *)
  ; ranks  : int              (* Number of remaining free variables. *)
  ; values : 'a array -> 'b } (* Substitution function. *)

(* Obtain the arity of a multi-binder. *)
let mbinder_arity : ('a,'b) mbinder -> int =
  fun mb -> Array.length mb.names

(* Get the names of the bound variables. *)
let mbinder_names : ('a,'b) mbinder -> string array =
  fun mb -> mb.names

(* The substitution function (again just an application). *)
let msubst : ('a,'b) mbinder -> 'a array -> 'b =
  fun mb xs -> mb.values xs

(* Do the varibles occur? *)
let mbinder_occurs : ('a,'b) mbinder -> bool array =
  fun mb -> mb.binds

(* Is the multi-binder constant? *)
let mbinder_constant : ('a,'b) mbinder -> bool =
  fun mb -> Array.fold_left (&&) true mb.binds

(* Is the multi-binder closed? *)
let mbinder_closed : ('a,'b) mbinder -> bool =
  fun mb -> mb.ranks = 0

(* How many free variables does the binder contain? *)
let mbinder_ranks : ('a,'b) mbinder -> int =
  fun mb -> mb.ranks

(* Split a variable name into a string and in integer suffix. If there is no
integer suffix, then -1 is given as the suffig integer. *)
let split_name : string -> string * int = fun name ->
  let n = String.length name in
  let p = ref (n-1) in
  let digit c = '0' <= c && c <= '9' in
  while !p >= 0 && digit name.[!p] do decr p done;
  let p = !p + 1 in
  if p = n || p = 0 then (name, (-1))
  else (String.sub name 0 p, int_of_string (String.sub name p (n - p)))

(* Merge a prefix and a suffix to form a name. *)
let merge_name : string -> int -> string = fun prefix suffix ->
  if suffix >= 0 then prefix ^ (string_of_int suffix) else prefix

(* We need a counter to have fresh keys for variables. *)
let fresh_key, reset_counter = new_counter ()

(* Generalise the type of a variable to fit in a bindbox. *)
let generalise_var : 'a variable -> any variable = Obj.magic

(* Dummy bindbox to be used prior to initialisation. *)
let dummy_bindbox : 'a bindbox =
  Open([], 0, fun _ -> failwith "Invalid use of dummy_bindbox")

(* Function for building a variable structure with a fresh key. *)
let build_new_var name mkfree bindbox =
  let (prefix, suffix) = split_name name in
  { key = fresh_key () ; var_name = name ; prefix = prefix ; suffix = suffix
  ; mkfree = mkfree ; bindbox = bindbox }

(* Create a new free variable using a wrapping function and a default name. *)
let new_var : ('a variable -> 'a) -> string -> 'a variable =
  let mk_var x htbl = (swap Env.get) (fst (IMap.find x.key htbl)) in
  fun mkfree name ->
    let x = build_new_var name mkfree dummy_bindbox in
    x.bindbox <- Open([generalise_var x], 0, mk_var x); x

(* Test if a variable occurs in a bindbox. *)
let occur v = function
  | Closed _     -> false
  | Open(vt,_,_) -> List.exists (fun v' -> v'.key = v.key) vt

(* Test if a bindbox is closed. *)
let is_closed = function
  | Closed _ -> true
  | _        -> false

(* List the name of the variables in a bindbox (for debugging). *)
let list_variables = function
  | Closed _     -> []
  | Open(vt,_,_) -> List.map (fun x -> x.var_name) vt

(* Put a term in a bindbox (no variables will be available to bind in the
given term. *)
let box : 'a -> 'a bindbox =
  fun t -> Closed t

(* Find a non-colliding suffix given a list of variables keys with name
collisions, the hashtable containing the corresponding suffixes (and the
positioning in the environment of the variables), and a prefered suffix. *)
let get_suffix collision htbl suffix =
  let l = List.map (fun key -> snd (IMap.find key htbl)) collision in
  let l = List.sort (-) l in
  let rec search suffix = function
    | x::l when x < suffix -> search suffix l
    | x::l when x = suffix -> search (suffix+1) l
    | _                    -> suffix
  in
  search suffix l

(* Merge two sorted list of variables withoug repetition into a sorted list
without repetition. *)
let rec merge l1 l2 =
  match (l1, l2) with
  | ([]   , _    ) -> l2
  | (_    , []   ) -> l1
  | (x::xs, y::ys) -> let kx = x.key and ky = y.key in
                      if kx = ky then merge xs l2
                      else if kx < ky then x :: merge xs l2
                      else y :: merge l1 ys

(* Search for all the colliding variables in a list of variables sorted by
keys. *)
let search x l =
  let k = x.key in
  let rec fn acc = function 
    | v::l when v.key < k -> fn (v::acc) l
    | v::l when v.key = k -> List.rev_append acc l
    | _                   -> raise Not_found
  in 
  fn [] l












(* FIXME from here *)
(* take a function t and an environment v and create an environment nv
   with places for future values of bound variables that t needs. 

   table.(i) gives the position in v of the variable with index i in nv
   table.(0) is unused.
   esize is the total size of nv
   nv.(0) is filled with the first unitialized position in nv which is
   Array.length table.
*)
let select frees nbbound t = 
  let mk_select t nbbound frees uptbl =
    let table = Array.make (List.length frees + 1) 0 in
    let cur = ref 0 in
    let downtbl = List.fold_left (fun htbl var ->
      incr cur;
      let upindex, suffix = IMap.find var.key uptbl in
      table.(!cur) <- upindex;
      IMap.add var.key (!cur,suffix) htbl) IMap.empty frees
    in
    let aux t esize table v =
      let nsize = Array.length table in
      let nv = Env.create esize in
      Env.set_next nv nsize;
      for i = 1 to nsize - 1 do
        Env.set nv i (Env.get v table.(i))
      done;
      t nv
    in
    aux (t downtbl) (!cur + nbbound + 1) table
  in
  if nbbound = 0 then t else mk_select t nbbound frees

(* The "apply" function of the monad. It takes as input a function with some
free variables, and an argument with some free variables and it builds the
application which might also have free variables. The function [select] is
used here to build the "minimal" closure when both the function and the
argument have free variables. *)
let apply_box tf ta =
  match (tf, ta) with
  | (Closed f     , Closed a     ) -> Closed (f a)
  | (Closed f     , Open(va,na,a)) -> Open(va, na, fun h v -> f (a h v))
  | (Open(vf,nf,f), Closed a     ) -> Open(vf, nf, fun h v -> (f h v) a)
  | (Open(vf,nf,f), Open(va,na,a)) ->
      let f = select vf nf f in
      let a = select va na a in
      Open(merge vf va, 0, fun h v -> (f h v) (a h v))

(* used for a binder when you bind a variable in closed term (therefore that *)
(* variable does not occur in the term ! *)
let mk_closed_bind v pt = { name = v.var_name; rank = 0; bind = false; value = fun _ -> pt}

(* used for binder which binds a variable with no occurrence (but the term has*)
(* other free variables *)
let mk_mute_bind rank name pt v = { name; rank; bind = false; value = fun _ -> pt v }

let mk_mute_bind2 rank collision prefix suffix pt htbl = 
  let suffix = get_suffix collision htbl suffix in
  let name = merge_name prefix suffix in
  mk_mute_bind rank name (pt htbl)

(* used for the first binder in a closed term (the binder that binds the last*)
(* free variable in a term and make it a closed term *)
let mk_first_bind2 rank prefix suffix key esize pt = 
  let mk_first_bind esize pt arg =
    let v = Env.create esize in
    Env.set_next v 2; Env.set v 0 arg;
    pt v
  in
  let htbl = IMap.empty in
  let htbl = IMap.add key (1,suffix) htbl in
  { name = merge_name prefix suffix; rank; bind = true; value = mk_first_bind esize (pt htbl) }

(* used for the general case of a binder *)
let mk_bind name pos pt v =
  { name; rank = pos - 1; bind = true; value = fun arg ->
  let next = Env.next v in 
  if next = pos then begin
    Env.set v next arg;
    Env.set_next v (next + 1);
    pt v
  end else begin
    let v = Env.dup v in 
    Env.set_next v (pos + 1);
    Env.set v pos arg;
    for i = pos + 1 to next - 1 do
      Env.set v i 0
    done;
    pt v
        end}

let mk_bind2 collision prefix suffix key pos pt htbl =
  let suffix = get_suffix collision htbl suffix in
  let name = merge_name prefix suffix in
  let htbl = IMap.add key (pos, suffix) htbl in
  mk_bind name pos (pt htbl)

let filter_map cond fn l =
  let rec gn acc = function
      [] -> List.rev acc
    | x::l -> if cond x then gn (fn x::acc) l else gn acc l
  in gn [] l

let bind_aux v t = match t with 
     Closed(t) ->
       Closed (mk_closed_bind v t)
   | Open(vt,nbt,t) -> 
       try 
         match vt with
          [var] -> 
            if v.key <> var.key then raise Not_found;
            let esize = nbt + 2 in
            Closed (mk_first_bind2 (List.length vt) v.prefix v.suffix v.key esize t)
        | _ ->
            let vt = search v vt in
            let collision = filter_map (fun v' -> v.prefix = v'.prefix) 
                (fun v -> v.key) vt in
            let pos = List.length vt + 1 in
            Open(vt, nbt+1, mk_bind2 collision v.prefix v.suffix v.key pos t)
      with Not_found ->
        let collision = filter_map (fun v' -> v.prefix = v'.prefix) 
            (fun v -> v.key) vt in
        Open(vt, nbt, mk_mute_bind2 (List.length vt) collision v.prefix v.suffix t)

(* take a function of type ('a bindbox -> 'b bindbox) and transform it into a binder*) 
(* of type ('a, 'b) binder bindbox *)
let bind bv name fpt =
  let v = new_var bv name in
  bind_aux v (fpt v.bindbox)

let bind_var = bind_aux

let mk_mbind names pos access pt v = 
  { names; ranks = pos-1; binds = access; values = fun args ->
    let arity = Array.length names in
    let size = Array.length args in
    if size <> arity then raise (Invalid_argument "bad arity in msubst");
    let next = Env.next v in
    let cur_pos = ref pos in
    if next = pos then begin
      for i = 0 to arity - 1 do
        if access.(i) then begin
          Env.set v !cur_pos args.(i);
          incr cur_pos;
        end
      done;
      Env.set_next v !cur_pos;
      pt v
    end else begin
      let v = Env.dup v in 
      for i = 0 to arity - 1 do
        if access.(i) then begin
          Env.set v !cur_pos args.(i);
          incr cur_pos;
        end
      done;
      Env.set_next v !cur_pos;
      for i = !cur_pos to next - 1 do
        Env.set v i 0
      done;
      pt v
    end }

let mk_mbind2 colls prefixes suffixes keys pos pt htbl =
  let cur_pos = ref pos in
  let htbl = ref htbl in
  let new_names = Array.make (Array.length prefixes) "" in
  let access = Array.mapi (fun i key ->
    let suffix = get_suffix colls.(i) !htbl suffixes.(i) in
    new_names.(i) <- merge_name prefixes.(i) suffix;
    if key <> 0 then begin
      htbl := IMap.add key (!cur_pos,suffix) !htbl;
      incr cur_pos;
      true
    end else
      false
      ) keys
  in
  mk_mbind new_names pos access (pt !htbl)

let mk_closed_mbind names t = 
  {names;
   ranks = 0;
   binds = Array.map (fun _ -> false) names;
   values = fun args ->
    let arity = Array.length names in
    let size = Array.length args in
    if size <> arity then raise (Invalid_argument "bad arity in msubst");
    t}

let mk_mute_mbind ranks names pt v = 
  {names;
   ranks;
   binds = Array.map (fun _ -> false) names;
   values = fun args ->
    let arity = Array.length names in
    let size = Array.length args in
    if size <> arity then raise (Invalid_argument "bad arity in msubst");
    pt v}

let mk_mute_mbind2 ranks colls prefixes suffixes pt htbl =
  let new_names = Array.make (Array.length prefixes) "" in
  Array.iteri (fun i c ->
    let suffix = get_suffix c htbl suffixes.(i) in
    new_names.(i) <- merge_name prefixes.(i) suffix;
              ) colls;
  mk_mute_mbind ranks new_names (pt htbl)

let mk_first_mbind names size access pt = {
  names; binds = access; ranks = 0; values =  fun args ->
  let v = Env.create size in
  let arity = Array.length names in
  let size = Array.length args in
  if size <> arity then raise (Invalid_argument "bad arity in msubst");
  let cur_pos = ref 1 in
  for i = 0 to arity - 1 do
    if access.(i) then begin
      Env.set v !cur_pos args.(i);
      incr cur_pos;
    end
  done;
  Env.set_next v !cur_pos;
  pt v}

let mk_first_mbind2 colls prefixes suffixes keys size pt =
  let cur_pos = ref 1 in
  let htbl = ref IMap.empty in
  let new_names = Array.make (Array.length prefixes) "" in
  let access = Array.mapi (fun i key ->
    let suffix = get_suffix colls.(i) !htbl suffixes.(i) in
    new_names.(i) <- merge_name prefixes.(i) suffix;
    if key <> 0 then begin
      htbl := IMap.add key (!cur_pos,suffix) !htbl;
      incr cur_pos;
      true
    end else
      false
      ) keys
  in
  mk_first_mbind new_names size access (pt !htbl)

let mbind_aux vs t = 
  match t with 
     Closed(t) ->
       let names = Array.map (fun v -> v.var_name) vs in
       Closed (mk_closed_mbind names t)
   | Open(vt,nbt,t) -> 
       let vt = ref vt in
       let nnbt = ref nbt in
       let len = Array.length vs in
       let prefixes = Array.make len "" in 
       let suffixes = Array.make len (-1) in 
       let colls = Array.make len [] in 
       let keys = Array.make len 0 in 
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
         let vt = !vt in
         Open(vt,nbt,mk_mute_mbind2 (List.length vt) colls prefixes suffixes t)
       else
         let pos = List.length !vt + 1 in
         Open(!vt,!nnbt,mk_mbind2 colls prefixes suffixes keys pos t)

(* take a function of type ('a bindbox array -> 'b bindbox) and transform it into a binder*) 
(* of type ('a, 'b) mbinder open_term *)

let new_mvar bv names =
  Array.map (fun name -> new_var bv name) names

let mbind bv names fpt =
  let vs = new_mvar bv names in
  let args = Array.map box_of_var vs in
  mbind_aux vs (fpt args)

let bind_mvar = mbind_aux

(* When a term has no free variable, you can get it ! *)
let unbox : 'a bindbox -> 'a = function
  | Closed(t) -> t
  | Open(vt,nbt,t) -> 
      let next =  List.length vt + 1 in
      let esize = next + nbt in
      let env = Env.create esize in
      Env.set_next env next;
      let cur = ref 0 in
      let aux htbl var =
        incr cur;
        Env.set env !cur (var.mkfree var);
        let _, suffix = split_name var.var_name in
        IMap.add var.key (!cur, suffix) htbl
      in
      let htbl = List.fold_left aux IMap.empty vt in
      t htbl env






(* Here are some usefull function *)
(* Some of them are optimised (the comment is the simple definition) *)

(*
let box_apply f ta = apply (unit f) ta
*)
let box_apply f ta =
  match ta with
    Closed(a) -> Closed (f a)
  | Open(va,ba,a) -> 
      Open(va, ba, fun h v -> f (a h v))

let mk_uapply f a b v = f (a v) (b v)
let mk_luapply f a b v = f a (b v)
let mk_ruapply f a b v = f (a v) b

let mk_uapply2 f a b h = mk_uapply f (a h) (b h)
let mk_luapply2 f a b h = mk_luapply f a (b h)
let mk_ruapply2 f a b h = mk_ruapply f (a h) b

let box_apply2 f ta tb =
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

let box_apply3 f t t' t'' = apply_box (box_apply2 f t t') t''

(* Used in some cases ! *)
let bind_apply f = box_apply2 (fun f -> f.value) f

let mbind_apply f = box_apply2 (fun f -> f.values) f

let rec cfix t = t (cfix t)

let rec fix t env =  (t env).value (fix t env)

let fix2 t htbl = fix (t htbl)

let fixpoint (f : (('a, 'b) binder, ('a, 'b) binder) binder bindbox) = 
  (match f with
    Closed t -> Closed(cfix t.value)
  | Open(vt,nbt,t) -> Open(vt,nbt,fix2 t) : ('a, 'b) binder bindbox)

(* dirty imperative functions ... *)

let special_apply tf ta =
  match (tf, ta) with
  | (Closed(())   , Closed(a)    ) ->
      (Closed (()), (fun _ _ -> a))
  | (Closed(())   , Open(va,na,a)) ->
      (Open(va,na,fun _ _ -> ()), select va na a)
  | (Open(vf,nf,_), Closed(a)    ) ->
      (tf, (fun _ _ -> a))
  | (Open(vf,nf,_), Open(va,na,a)) ->
      (Open(merge vf va, 0, fun _ _ -> ()), select va na a)
   
let special_end e f =
  match e with
  | Closed _    -> Closed(f IMap.empty (Env.create 0))
  | Open(v,b,_) -> Open(v,b,f)

(* to get very nice and efficient functor !*)

module type Map = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Lift(M : Map) = struct
  let f t =
    let acc = ref (Closed ()) in
    let fn o =
      let nacc, o = special_apply !acc o in
            acc := nacc; o
    in
    let t = M.map fn t in
    let aux htbl =
      let l = M.map (fun o -> o htbl) t in
      (fun env -> M.map (fun o -> o env) l)
    in special_end !acc aux
end

module type Map2 = sig
  type ('a, 'b) t
  val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
end

module Lift2(M: Map2) = struct
  let f t =
    let acc = ref (Closed ()) in
    let fn o =
      let nacc, o = special_apply !acc o in
            acc := nacc; o
    in
    let t = M.map fn fn t in
    let aux htbl =
      let l = M.map (fun o -> o htbl) (fun o -> o htbl) t in
      (fun env -> M.map (fun o -> o env) (fun o -> o env) l)
    in
    special_end !acc aux
  end


module Lift_list = Lift(
  struct
    type 'a t = 'a list
    let map = List.map  
  end)
let box_list = Lift_list.f

module Lift_rev_list = Lift(
  struct
    type 'a t = 'a list
    let map = List.rev_map  
  end)
let box_rev_list = Lift_rev_list.f

module Lift_array = Lift(
  struct
    type 'a t = 'a array
    let map = Array.map 
  end)
let box_array = Lift_array.f



let box_pair x y = box_apply2 (fun x y -> x,y) x y

                      
                          



(* Context *)
type context = int list SMap.t

let empty_context = SMap.empty

let new_var_in ctxt (bv  : 'a variable -> 'a) name =
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
  in
  let prefix, suffix = split_name name in
  let new_suffix, ctxt = get_suffix prefix suffix ctxt in
  let rec var = { 
    key = fresh_key ();
    var_name = merge_name prefix new_suffix;
    prefix = prefix;
    suffix = suffix;
    mkfree = bv; 
    bindbox = dummy_bindbox }
  in
  let mk_var x htbl = (swap Env.get) (fst (IMap.find x.key htbl)) in
  let result = Open([generalise_var var], 0, mk_var var) in
  var.bindbox <- result;
  var, ctxt

let bind_in ctxt bv name fpt =
  let v, ctxt = new_var_in ctxt bv name in
  bind_aux v (fpt v.bindbox ctxt)

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

let mbind_in ctxt bv names fpt =
  let vs,ctxt = new_mvar_in ctxt bv names in
  let args = Array.map box_of_var vs in
  mbind_aux vs (fpt args ctxt)
