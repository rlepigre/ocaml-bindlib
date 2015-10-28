(****************************************************************************
 * The Bindlib Library provides datatypes to represent binders in arbitrary *
 * languages. The implementation is efficient and manages name in the expe- *
 * ted way (variables have a prefered name to which an integer  suffix  can *
 * be added to avoid capture during substitution.                           *
 *                                                                          *
 * Author: Christophe Raffalli                                              *
 * Modified by: Rodolphe Lepigre                                            *
 ****************************************************************************)

open Bindlib_util

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

(* A function to apply a function under a bindbox. *)
let apply_in_box : ('a -> 'b) -> 'a bindbox -> 'b bindbox = fun f b ->
  match b with
  | Closed(e)    -> Closed(f e)
  | Open(vl,i,g) -> Open(vl,i,fun vp env -> f (g vp env))

(* Obtain the name of a the given variable. *)
let name_of : 'a variable -> string =
  fun x -> x.var_name

(* Safe comparison function for variables. *)
let compare_variables : 'a variable -> 'b variable -> int =
  fun x y -> Pervasives.compare x.key y.key
let eq_variables : 'a variable -> 'b variable -> bool =
  fun x y -> x.key = y.key

(* Hash function for variables. *)
let hash_var : 'a variable -> int =
  fun x -> Hashtbl.hash (`HVar, x.key)

(* Build a term with a free variable from a variable. *)
let free_of : 'a variable -> 'a =
  fun x -> x.mkfree x

(* Build a bindbox from a variable. *)
let box_of_var : 'a variable -> 'a bindbox =
  fun x -> x.bindbox

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

(* Compose a binder with a function. *)
let binder_compose_left  : ('a -> 'b) -> ('b,'c) binder -> ('a,'c) binder =
  fun f b ->
    let v x = b.value (f x) in
    { name = b.name ; bind = b.bind ; rank = b.rank ; value = v }

let binder_compose_right : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder =
  fun b f ->
    let v x = f (b.value x) in
    { name = b.name ; bind = b.bind ; rank = b.rank ; value = v }

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

(* Same function for multi-variables. *)
let new_mvar : ('a variable -> 'a) -> string array -> 'a mvariable =
  fun mkfree names -> Array.map (fun n -> new_var mkfree n) names

(* Make a copy of a variable with a potentially different name and syntactic
wrapper but with the same key. *)
let copy_var : 'b variable -> string -> ('a variable -> 'a) -> 'a variable =
  fun x name mkfree ->
    let y = new_var mkfree name in
    { y with key = x.key }

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

(* Transforms a "closure" so that a space is reserved for all the bound
variables. *)
let fn_select table nsize esize pt v =
  let nv = Env.create esize in
  Env.set_next nv nsize;
  for i = 0 to nsize - 1 do
    Env.set nv i (Env.get v table.(i))
  done;
  pt nv

let select vs nb t =
  if nb = 0 then t else (fun uptbl ->
    let nsize = List.length vs in
    let table = Array.make nsize 0 in
    let cur = ref 0 in
    let f htbl var =
      let i = !cur in incr cur;
      let (upindex, suffix) = IMap.find var.key uptbl in
      table.(i) <- upindex;
      IMap.add var.key (i,suffix) htbl
    in
    let downtbl = List.fold_left f IMap.empty vs in
    let esize = nsize + nb in
    fn_select table nsize esize (t downtbl))

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

(* Get out of the [bindbox] structure and construct an actual term. When the
term is closed, it is straight-forward, but when it is open, free variables
need to be made free in the syntax using the [mkfree] field. *)
let unbox : 'a bindbox -> 'a = function
  | Closed(t) -> t
  | Open(vt,nbt,t) ->
      let next = List.length vt in
      let esize = next + nbt in
      let env = Env.create esize in
      Env.set_next env next;
      let cur = ref 0 in
      let aux htbl var =
        let i = !cur in incr cur;
        Env.set env i (var.mkfree var);
        let _, suffix = split_name var.var_name in
        IMap.add var.key (i, suffix) htbl
      in
      let htbl = List.fold_left aux IMap.empty vt in
      t htbl env

(* Build a binder in the case it is the first binder in a closed term (i.e.
(the binder that binds the last free variable in a term and thus close it). *)
let value_first_bind esize pt arg =
  let v = Env.create esize in Env.set_next v 1;
  Env.set v 0 arg;
  pt v

let mk_first_bind rank x esize pt =
  let htbl = IMap.add x.key (0,x.suffix) IMap.empty in
  let pt = pt htbl in
  { name = merge_name x.prefix x.suffix; rank; bind = true
  ; value = value_first_bind esize pt }

(* Build a normal binder (the default case) *)
let value_bind pt pos v arg =
  let next = Env.next v in
  if next = pos then
    (Env.set v next arg; Env.set_next v (next + 1); pt v)
  else
    (let v = Env.dup v in
     Env.set_next v (pos + 1); Env.set v pos arg;
     for i = pos + 1 to next - 1 do Env.set v i 0 done; pt v)

let fn_bind pt pos name v =
  { name; rank = pos - 1; bind = true; value = value_bind pt pos v }

let mk_bind cols x pos pt htbl =
  let suffix = get_suffix cols htbl x.suffix in
  let name = merge_name x.prefix suffix in
  let htbl = IMap.add x.key (pos, suffix) htbl in
  let pt = pt htbl in
  fn_bind pt pos name

(* Build a normal binder for a non occuring variable *)
let value_mute_bind pt v arg = pt v

let fn_mute_bind name pos pt v =
  { name; rank = pos; bind = false; value = value_mute_bind pt v }

let mk_mute_bind cols x pos pt htbl =
  let pt = pt htbl in
  let name = merge_name x.prefix (get_suffix cols htbl x.suffix) in
  fn_mute_bind name pos pt

(* Binds the given variable in the given bindbox to produce a binder. *)
let bind_var x = function
  | Closed t ->
     Closed {name = x.var_name; rank = 0; bind = false; value = fun _ -> t}
  | Open(vs,nb,t) ->
     try
       match vs with
       | [y] -> if x.key <> y.key then raise Not_found;
                Closed (mk_first_bind (List.length vs) x (nb + 1) t)
       | _   -> let vt = search x vs in
                let eq_pref y = x.prefix = y.prefix in
                let cols = filter_map eq_pref (fun v -> v.key) vt in
                let pos = List.length vt in
                Open(vt, nb + 1, mk_bind cols x pos t)
    with Not_found ->
      let eq_pref y = x.prefix = y.prefix in
      let cols = filter_map eq_pref (fun v -> v.key) vs in
      let rank = List.length vs in
      Open(vs, nb, mk_mute_bind cols x rank t)

(* Transforms a function of type ['a bindbox -> 'b bindbox] into a binder. *)
let bind mkfree name f =
  let x = new_var mkfree name in
  bind_var x (f x.bindbox)

let unbind mkfree b =
  let x = new_var mkfree (binder_name b) in
  (x, subst b (free_of x))

let value_mbind arity binds pos pt v args =
  let size = Array.length args in
  if size <> arity then raise (Invalid_argument "bad arity in msubst");
  let next = Env.next v in
  let cur_pos = ref pos in
  if next = pos then begin
    for i = 0 to arity - 1 do
      if binds.(i) then begin
        Env.set v !cur_pos args.(i);
        incr cur_pos;
      end
    done;
    Env.set_next v !cur_pos;
    pt v
  end else begin
    let v = Env.dup v in
    for i = 0 to arity - 1 do
      if binds.(i) then begin
        Env.set v !cur_pos args.(i);
        incr cur_pos;
      end
    done;
    Env.set_next v !cur_pos;
    for i = !cur_pos to next - 1 do Env.set v i 0 done;
    pt v
  end

let fn_mbind names arity binds pos pt v =
  { names; ranks = pos-1; binds; values = value_mbind arity binds pos pt v }

(* Auxiliary functions *)
let mk_mbind colls prefixes suffixes keys pos pt htbl =
  let cur_pos = ref pos in
  let htbl = ref htbl in
  let names = Array.make (Array.length prefixes) "" in
  let f i key =
    let suffix = get_suffix colls.(i) !htbl suffixes.(i) in
    names.(i) <- merge_name prefixes.(i) suffix;
    if key <> 0 then
      (htbl := IMap.add key (!cur_pos,suffix) !htbl; incr cur_pos; true)
    else false
  in
  let binds = Array.mapi f keys in
  let arity = Array.length names in
  let pt = pt !htbl in
  fn_mbind names arity binds pos pt

let value_mute_mbind arity pt v args =
  if arity <> Array.length args then
    raise (Invalid_argument "bad arity in msubst");
  pt v

let fn_mute_mbind arity names binds pos pt v =
  {names; ranks = pos; binds; values = value_mute_mbind arity pt v}

let mk_mute_mbind pos colls prefixes suffixes pt htbl =
  let new_names = Array.make (Array.length prefixes) "" in
  let f i c =
    let suffix = get_suffix c htbl suffixes.(i) in
    new_names.(i) <- merge_name prefixes.(i) suffix
  in
  Array.iteri f colls;
  let arity = Array.length new_names in
  let pt = pt htbl in
  let binds = Array.map (fun _ -> false) new_names in
  fn_mute_mbind arity new_names binds pos pt

let value_first_mbind arity binds esize pt args =
  let v = Env.create esize in
  if Array.length args <> arity then
    raise (Invalid_argument "bad arity in msubst");
  let cur_pos = ref 0 in
  for i = 0 to arity - 1 do
    if binds.(i) then begin
      Env.set v !cur_pos args.(i);
      incr cur_pos;
    end
  done;
  Env.set_next v !cur_pos;
  pt v

let mk_first_mbind colls prefixes suffixes keys esize pt =
  let cur_pos = ref 0 in
  let htbl = ref IMap.empty in
  let names = Array.make (Array.length prefixes) "" in
  let f i key =
    let suffix = get_suffix colls.(i) !htbl suffixes.(i) in
    names.(i) <- merge_name prefixes.(i) suffix;
    if key <> 0 then
      (htbl := IMap.add key (!cur_pos,suffix) !htbl; incr cur_pos; true)
    else false
  in
  let binds = Array.mapi f keys in
  let arity = Array.length names in
  let pt = pt !htbl in
  {names; binds; ranks = 0; values = value_first_mbind arity binds esize pt}

(* Bind a multi-variable in a bindbox to produce a multi-binder. *)
let bind_mvar vs = function
  | Closed t ->
      let names = Array.map (fun v -> v.var_name) vs in
      let values args =
        if Array.length names <> Array.length args then
          raise (Invalid_argument "bad arity in msubst");
        t
      in
      let binds = Array.map (fun _ -> false) names in
      Closed {names; ranks = 0; binds ; values}
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
        Closed(mk_first_mbind colls prefixes suffixes keys !nnbt t)
      else if !nnbt = nbt then
        let vt = !vt in
        Open(vt,nbt,mk_mute_mbind (List.length vt) colls prefixes suffixes t)
      else
        let pos = List.length !vt in
        Open(!vt,!nnbt,mk_mbind colls prefixes suffixes keys pos t)

(* Take a function of type ['a bindbox array -> 'b bindbox] and builds the
corresponing multi-binder. *)
let mbind mkfree names f =
  let vs = new_mvar mkfree names in
  let args = Array.map box_of_var vs in
  bind_mvar vs (f args)

(* Optimized equivalent of [let box_apply f ta = apply (unit f) ta]. *)
let box_apply f = function
  | Closed a      -> Closed (f a)
  | Open(va,na,a) -> Open(va,na,fun h v -> f (a h v))

let box_apply2 f ta tb =
  match (ta, tb) with
  | (Closed(a)    , Closed (b)   ) -> Closed (f a b)
  | (Closed(a)    , Open(vb,bb,b)) -> Open(vb,bb,fun h v -> f a (b h v))
  | (Open(va,ba,a), Closed(b)    ) -> Open(va,ba,fun h v -> f (a h v) b)
  | (Open(va,ba,a), Open(vb,bb,b)) ->
      let a = select va ba a in
      let b = select vb bb b in
      Open(merge va vb, 0, fun h v -> f (a h v) (b h v))

let box_apply3 f ta tb tc = apply_box (box_apply2 f ta tb) tc

let bind_apply f = box_apply2 (fun f -> f.value) f

let mbind_apply f = box_apply2 (fun f -> f.values) f

let fixpoint = function
  | Closed t      -> let rec fix t =
                       t (fix t)
                     in Closed(fix t.value)
  | Open(vs,nb,t) ->
     let fix t htbl =
       let t = t htbl in
       let rec fix' env = (t env).value (fix' env) in fix'
     in Open(vs, nb, fix t)

(* Functorial interface to generate lifting functions for [bindbox]. *)
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

module type Map = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Map2 = sig
  type ('a, 'b) t
  val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
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
    in
    match !acc with
    | Closed _    -> Closed(aux IMap.empty (Env.create 0))
    | Open(v,b,_) -> Open(v,b,aux)
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
    match !acc with
    | Closed _    -> Closed(aux IMap.empty (Env.create 0))
    | Open(v,b,_) -> Open(v,b,aux)
  end

(* Some standard lifting functions. *)
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

let box_opt = function
  | None   -> box None
  | Some b -> box_apply (fun b -> Some b) b

(* Type of a context. *)
type context = int list SMap.t

(* The empty context. *)
let empty_context = SMap.empty

(* Equivalent of [new_var] in a context. *)
let new_var_in ctxt mkfree name =
  let get_suffix name suffix ctxt =
    try
      let l = SMap.find name ctxt in
      let rec search acc suf = function
        | []                     -> (suf, List.rev_append acc [suf])
        | x::_ as l when x > suf -> (suf, List.rev_append acc (suf::l))
        | x::l when x = suf      -> search (x::acc) (suf+1) l
        | x::l (*x < suf *)      -> search (x::acc) suf l
      in
      let (suffix, l) = search [] suffix l in
      (suffix, SMap.add name l ctxt)
    with Not_found -> (suffix, SMap.add name [suffix] ctxt)
  in
  let (prefix, suffix) = split_name name in
  let (new_suffix, ctxt) = get_suffix prefix suffix ctxt in
  let var_name = merge_name prefix new_suffix in
  let var =
    { key = fresh_key () ; var_name ; prefix; suffix = new_suffix
    ; mkfree ; bindbox = dummy_bindbox }
  in
  let mk_var x htbl = (swap Env.get) (fst (IMap.find x.key htbl)) in
  let result = Open([generalise_var var], 0, mk_var var) in
  var.bindbox <- result;
  (var, ctxt)

(* Equivalent of [bind] in a context. *)
let bind_in ctxt mkfree name f =
  let (v, ctxt) = new_var_in ctxt mkfree name in
  bind_var v (f v.bindbox ctxt)

(* Equivalent of [new_mvar] in a context. *)
let new_mvar_in ctxt mkfree names =
  let ctxt = ref ctxt in
  let f name =
    let (v, new_ctxt) = new_var_in !ctxt mkfree name in
    ctxt := new_ctxt; v
  in
  let vs = Array.map f names in
  (vs, !ctxt)

(* Equivalent of [mbind] in a context. *)
let mbind_in ctxt mkfree names fpt =
  let (vs, ctxt) = new_mvar_in ctxt mkfree names in
  let args = Array.map box_of_var vs in
  bind_mvar vs (fpt args ctxt)
