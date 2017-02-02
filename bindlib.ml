(****************************************************************************
 * The Bindlib Library provides datatypes to represent binders in arbitrary *
 * languages. The implementation is efficient and manages name in the expe- *
 * ted way (variables have a prefered name to which an integer  suffix  can *
 * be added to avoid capture during substitution.                           *
 *                                                                          *
 * Author: Christophe Raffalli                                              *
 * Modified by: Rodolphe Lepigre                                            *
 ****************************************************************************)

type any = Obj.t

(* An environment is used to store the value of every bound variables. We
   need to use Obj since bound variables may have different types and we
   want to store them in a single array. *)
module Env = struct
  type t =
    { tab          : any array (* An array with elements of any type. *)
    ; mutable next : int }     (* Next free cell of the array. *)

  (* Creates an empty environment of a given size. *)
  let create : int -> t =
    fun size ->
      let dummy = Obj.repr () in
      { tab = Array.make size dummy; next = 0 }

  (* Sets the value stored at some position in the environment. *)
  let set : t -> int -> 'a -> unit =
    fun env i e -> Array.set env.tab i (Obj.repr e)

  (* Gets the value stored at some position in the environment. *)
  let get : int -> t -> 'a =
    fun i env -> Obj.obj (Array.get env.tab i)

  (* Make a copy of the environment. *)
  let dup : t -> t =
    fun env -> { tab = Array.copy env.tab; next = env.next }

  (* Get next free cell index. *)
  let next : t -> int =
    fun env -> env.next

  (* Set the next free cell index. *)
  let set_next : t -> int -> unit =
    fun env n -> env.next <- n
end

module IMap :
  sig
    type 'a t
    val empty : 'a t
    val add   : int -> 'a -> 'a t -> 'a t
    val find  : int -> 'a t -> 'a
  end =
  struct
    (** Maps over integers implemented as Patricia trees.
        Copyright (C) 2000 Jean-Christophe FILLIATRE *)
    type 'a t =
      | Empty
      | Leaf of int * 'a
      | Branch of int * int * 'a t * 'a t

    let empty = Empty

    let rec mem k = function
      | Empty -> false
      | Leaf (j,_) -> k == j
      | Branch (_, m, l, r) -> mem k (if k land m == 0 then l else r)

    let rec find k = function
      | Empty -> raise Not_found
      | Leaf (j,x) -> if k == j then x else raise Not_found
      | Branch (_, m, l, r) -> find k (if k land m == 0 then l else r)

    let lowest_bit x = x land (-x)

    let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

    let mask p m = p land (m-1)

    let join (p0,t0,p1,t1) =
      let m = branching_bit p0 p1 in
      if p0 land m == 0 then Branch (mask p0 m, m, t0, t1)
      else Branch (mask p0 m, m, t1, t0)

    let match_prefix k p m = (mask k m) == p

    let add k x t =
      let rec ins = function
        | Empty                   -> Leaf (k,x)
        | Leaf (j,_) as t         -> if j == k then Leaf (k,x)
                                     else join (k, Leaf (k,x), j, t)
        | Branch (p,m,t0,t1) as t ->
            if match_prefix k p m then
              if k land m == 0 then Branch (p, m, ins t0, t1)
              else Branch (p, m, t0, ins t1)
            else join (k, Leaf (k,x), p, t)
      in ins t
  end

module SMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

let new_counter =
  let c = ref 0 in
  fun () ->
    let fresh () = let n = !c in incr c; n in
    let reset () = c := 0 in
    (fresh, reset)

let filter_map cond fn l =
  let rec aux acc = function
    | []   -> List.rev acc
    | x::l -> if cond x then aux (fn x::acc) l else aux acc l
  in aux [] l

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
  | Open of any var list * int * (varpos -> Env.t -> 'a)

(* Type of a free variable of type ['a]. *)
and 'a var =
  { key             : int     (* Unique identifier. *)
  ; prefix          : string  (* Name as a free var. *)
  ; suffix          : int    (* Prefix, with integer suffix. *)
  ; mkfree          : 'a var -> 'a (* Function to build a term. *)
  ; mutable bindbox : 'a bindbox } (* Bindbox containing the variable. *)

(* A function to apply a function under a bindbox. *)
let apply_in_box : ('a -> 'b) -> 'a bindbox -> 'b bindbox = fun f b ->
  match b with
  | Closed(e)    -> Closed(f e)
  | Open(vl,i,g) -> Open(vl,i,fun vp env -> f (g vp env))

(* Merge a prefix and a suffix to form a name. *)
let merge_name : string -> int -> string = fun prefix suffix ->
  if suffix >= 0 then prefix ^ (string_of_int suffix) else prefix

(* Obtain the name of a the given variable. *)
let name_of : 'a var -> string =
  fun x -> merge_name x.prefix x.suffix
let prefix_of : 'a var -> string =
  fun x -> x.prefix

(* Safe comparison function for variables. *)
let compare_vars : 'a var -> 'b var -> int =
  fun x y -> y.key - x.key
let eq_vars : 'a var -> 'b var -> bool =
  fun x y -> x.key = y.key

(* Hash function for variables. *)
let hash_var : 'a var -> int =
  fun x -> Hashtbl.hash (`HVar, x.key)

(* Build a term with a free variable from a variable. *)
let free_of : 'a var -> 'a =
  fun x -> x.mkfree x

(* Build a bindbox from a variable. *)
let box_of_var : 'a var -> 'a bindbox =
  fun x -> x.bindbox

(* Type of multi-variables of type ['a]. *)
type 'a mvar = 'a var array

(* Type of a binder, i.e. an expression of type ['b] with a bound variable of
type ['a]. *)
type ('a,'b) binder =
  { name   : string     (* Name of the bound variable. *)
  ; bind   : bool       (* Does the variable occur? *)
  ; rank   : int        (* Number of remaining free variables (>= 0). *)
  ; value  : 'a -> 'b } (* Substitution function. *)

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
    { name = b.name ;
      bind = b.bind ; rank = b.rank ; value = v }

let binder_compose_right : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder =
  fun b f ->
    let v x = f (b.value x) in
    { name = b.name ;
      bind = b.bind ; rank = b.rank ; value = v }

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
integer suffix, then -1 is given as the suffix integer. *)
let split_name : string -> string * int = fun name ->
  let n = String.length name in
  let p = ref (n-1) in
  let digit c = '0' <= c && c <= '9' in
  while !p >= 0 && digit name.[!p] do decr p done;
  let p = !p + 1 in
  if p = n || p = 0 then (name, (-1))
  else (String.sub name 0 p,
        int_of_string (String.sub name p (n - p)))

(* We need a counter to have fresh keys for variables. *)
let fresh_key, reset_counter = new_counter ()

(* Generalise the type of a variable to fit in a bindbox. *)
let generalise_var : 'a var -> any var = Obj.magic

(* Dummy bindbox to be used prior to initialisation. *)
let dummy_bindbox : 'a bindbox =
  Open([], 0, fun _ -> failwith "Invalid use of dummy_bindbox")

(* Function for building a variable structure with a fresh key. *)
let build_new_var name mkfree bindbox =
  let prefix, suffix = split_name name in
  { key = fresh_key () ; prefix; suffix ; mkfree; bindbox }

let update_var_bindbox : 'a var -> unit =
  let mk_var x htbl = Env.get (fst (IMap.find x.key htbl)) in
  fun x -> x.bindbox <- Open([generalise_var x], 0, mk_var x)

(* Create a new free variable using a wrapping function and a default name. *)
let new_var : ('a var -> 'a) -> string -> 'a var =
  fun mkfree name ->
    let x = build_new_var name mkfree dummy_bindbox in
    update_var_bindbox x; x

(* Same function for multi-variables. *)
let new_mvar : ('a var -> 'a) -> string array -> 'a mvar =
  fun mkfree names -> Array.map (fun n -> new_var mkfree n) names

(* Make a copy of a variable with a potentially different name and syntactic
wrapper but with the same key. *)
let copy_var : 'b var -> string -> ('a var -> 'a) -> 'a var =
  fun x name mkfree ->
    let y = new_var mkfree name in
    let r = { y with key = x.key } in
    update_var_bindbox r;
    r

(* Test if a variable occurs in a bindbox. *)
let occur v = function
  | Closed _     -> false
  | Open(vt,_,_) -> List.exists (fun v' -> v'.key = v.key) vt

(* Test if a bindbox is closed. *)
let is_closed = function
  | Closed _ -> true
  | _        -> false

(* List the name of the variables in a bindbox (for debugging). *)
let list_vars = function
  | Closed _     -> []
  | Open(vt,_,_) -> List.map (fun x -> name_of x) vt

(* Put a term in a bindbox (no variables will be available to bind in the
given term. *)
let box : 'a -> 'a bindbox =
  fun t -> Closed t

(* Find a non-colliding suffix given a list of variables keys with name
collisions, the hashtable containing the corresponding suffixes (and the
positioning in the environment of the variables), and a prefered suffix. *)
let get_suffix : 'a. any var list -> varpos -> 'a var -> int =
  fun vt htbl x ->
  let eq_pref (y:'a var) = x.prefix = y.prefix in
  let cols = filter_map eq_pref (fun (v:'a var) -> v.key) vt in
  let l = List.map (fun key -> snd (IMap.find key htbl)) cols in
  let l = List.sort (-) l in
  let rec search suffix = function
    | x::l when x < suffix -> search suffix l
    | x::l when x = suffix -> search (suffix+1) l
    | _                    -> suffix
  in
  search x.suffix l

(* Merge two sorted list of variables without repetition into a sorted list
without repetition. *)
let rec merge l1 l2 =
  match (l1, l2) with
  | ([]   , _    ) -> l2
  | (_    , []   ) -> l1
  | (x::xs, y::ys) -> let kx = x.key and ky = y.key in
                      if kx = ky then merge xs l2
                      else if kx < ky then x :: merge xs l2
                      else y :: merge l1 ys

(* Search for a variable in a list of variables sorted by keys. *)
let search x l =
  let k = x.key in
  let rec fn acc = function
    | v::l when v.key < k -> fn (v::acc) l
    | v::l when v.key = k -> List.rev_append acc l
    | _                   -> raise Not_found
  in
  fn [] l

(* Transforms a "closure" so that a space is reserved only for
   the occuring variables. *)
let fn_select table nsize esize pt v =
  let nv = Env.create esize in
  Env.set_next nv nsize;
  for i = 0 to nsize - 1 do
    Env.set nv i (Env.get table.(i) v)
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
  | (Closed f     , Open(va,na,a)) -> Open(va, na, fun h -> let a = a h in fun v -> f (a v))
  | (Open(vf,nf,f), Closed a     ) -> Open(vf, nf, fun h -> let f = f h in fun v -> f v a)
  | (Open(vf,nf,f), Open(va,na,a)) ->
      let f = select vf nf f in
      let a = select va na a in
      Open(merge vf va, 0, fun h -> let f = f h and a = a h in fun v -> f v (a v))

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
        let suffix = snd (split_name (name_of var)) in
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

let mk_first_bind x esize pt =
  let htbl = IMap.add x.key (0,x.suffix) IMap.empty in
  let pt = pt htbl in
  { name = merge_name x.prefix x.suffix; rank = 0; bind = true
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
  { name; rank = pos; bind = true; value = value_bind pt pos v }

let mk_bind vt x pos pt htbl =
  let suffix = get_suffix vt htbl x in
  let htbl = IMap.add x.key (pos, suffix) htbl in
  let pt = pt htbl in
  fn_bind pt pos (merge_name x.prefix suffix)

(* Build a normal binder for a non occuring variable *)
let value_mute_bind pt v arg = pt v

let fn_mute_bind pt pos name v =
  { name; rank = pos; bind = false; value = value_mute_bind pt v }

let mk_mute_bind vt (x:'a var) pos pt htbl =
  let suffix = get_suffix vt htbl x in
  let pt = pt htbl in
  fn_mute_bind pt pos (merge_name x.prefix suffix)

(* Binds the given variable in the given bindbox to produce a binder. *)
let bind_var : 'a 'b. 'a var -> 'b bindbox -> ('a, 'b) binder bindbox =
  fun x -> function
  | Closed t ->
     Closed {name = merge_name x.prefix x.suffix;
             rank = 0; bind = false; value = fun _ -> t}
  | Open(vs,nb,t) ->
     try
       match vs with
       | [y] -> if x.key <> y.key then raise Not_found;
                Closed (mk_first_bind x (nb + 1) t)
       | _   -> let vt = search x vs in
                let pos = List.length vt in
                Open(vt, nb + 1, mk_bind vt x pos t)
    with Not_found ->
      let rank = List.length vs in
      Open(vs, nb, mk_mute_bind vs x rank t)

(* Transforms a function of type ['a bindbox -> 'b bindbox] into a binder. *)
let bind mkfree name f =
  let x = new_var mkfree name in
  bind_var x (f x.bindbox)

let vbind mkfree name f =
  let x = new_var mkfree name in
  bind_var x (f x)

let unbind mkfree b =
  let x = new_var mkfree (binder_name b) in
  (x, subst b (free_of x))

let unmbind mkfree b =
  let x = new_mvar mkfree (mbinder_names b) in
  (x, msubst b (Array.map free_of x))

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

let fn_mbind names binds pos pt v =
  let arity = Array.length names in
  { names; ranks = pos; binds;
    values = value_mbind arity binds pos pt v }

(* Auxiliary functions *)
let mk_mbind vt vs keys pt htbl =
  let pos = List.length vt in
  let cur_pos = ref pos in
  let htbl = ref htbl in
  let arity = Array.length vs in
  let names = Array.make arity "" in
  let f i key =
    let suffix = get_suffix vt !htbl vs.(i) in
    names.(i) <- merge_name vs.(i).prefix suffix;
    if key >= 0 then
      (htbl := IMap.add key (!cur_pos,suffix) !htbl; incr cur_pos; true)
    else false
  in
  let binds = Array.mapi f keys in
  let pt = pt !htbl in
  fn_mbind names binds pos pt

let value_mute_mbind arity pt v args =
  if arity <> Array.length args then
    raise (Invalid_argument "bad arity in msubst");
  pt v

let fn_mute_mbind names binds pos pt v =
  let arity = Array.length names in
  {names; ranks = pos; binds; values = value_mute_mbind arity pt v}

let mk_mute_mbind vt vs pt htbl =
  let pos = List.length vt in
  let names = Array.map (fun v ->
    let suffix = get_suffix vt htbl v in
    merge_name v.prefix suffix) vs
  in
  let pt = pt htbl in
  let binds = Array.map (fun _ -> false) vs in
  fn_mute_mbind names binds pos pt

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

let mk_first_mbind vt vs keys esize pt =
  let cur_pos = ref 0 in
  let htbl = ref IMap.empty in
  let arity = Array.length vs in
  let names = Array.make arity "" in
  let f i key =
    let suffix = get_suffix vt !htbl vs.(i) in
    names.(i) <- merge_name vs.(i).prefix suffix;
    if key >= 0 then
      (htbl := IMap.add key (!cur_pos,suffix) !htbl; incr cur_pos; true)
    else false
  in
  let binds = Array.mapi f keys in
  let pt = pt !htbl in
  {names; binds; ranks = 0;
   values = value_first_mbind arity binds esize pt}

(* Bind a multi-variable in a bindbox to produce a multi-binder. *)
let bind_mvar vs = function
  | Closed t ->
      let values args =
        if Array.length vs <> Array.length args then
          raise (Invalid_argument "bad arity in msubst");
        t
      in
      let binds = Array.map (fun _ -> false) vs in
      let names = Array.map (fun x ->
                      merge_name x.prefix x.suffix) vs in
      Closed {names; ranks = 0; binds ; values}
  | Open(vt,nbt,t) ->
      let vt = ref vt in
      let nnbt = ref nbt in
      let len = Array.length vs in
      let keys = Array.make len 0 in
      for i = len - 1 downto 0 do
        let v = vs.(i) in
        try
          let ng = search v !vt in
          incr nnbt;
          vt := ng;
          keys.(i) <- v.key
        with Not_found ->
          keys.(i) <- -1
      done;
      let vt = !vt in
      if vt = [] then
        Closed(mk_first_mbind [] vs keys !nnbt t)
      else if !nnbt = nbt then
        Open(vt,nbt,mk_mute_mbind vt vs t)
      else
        Open(vt,!nnbt,mk_mbind vt vs keys t)

(* Take a function of type ['a bindbox array -> 'b bindbox] and builds the
corresponing multi-binder. *)
let mbind mkfree names f =
  let vs = new_mvar mkfree names in
  let args = Array.map box_of_var vs in
  bind_mvar vs (f args)

let mvbind mkfree names f =
  let vs = new_mvar mkfree names in
  bind_mvar vs (f vs)

(* Optimized equivalent of [let box_apply f ta = apply (unit f) ta]. *)
let box_apply f = function
  | Closed a      -> Closed (f a)
  | Open(va,na,a) -> Open(va,na,fun h -> let a = a h in (fun v -> f (a v)))

let box_apply2 f ta tb =
  match (ta, tb) with
  | (Closed(a)    , Closed (b)   ) -> Closed (f a b)
  | (Closed(a)    , Open(vb,bb,b)) -> Open(vb,bb,fun h -> let b = b h in fun v -> f a (b v))
  | (Open(va,ba,a), Closed(b)    ) -> Open(va,ba,fun h -> let a = a h in fun v -> f (a v) b)
  | (Open(va,ba,a), Open(vb,bb,b)) ->
      let a = select va ba a in
      let b = select vb bb b in
      Open(merge va vb, 0, fun h -> let a = a h and b = b h in fun v -> f (a v) (b v))

let box_apply3 f ta tb tc = apply_box (box_apply2 f ta tb) tc

let box_apply4 f ta tb tc td = apply_box (box_apply3 f ta tb tc) td

let bind_apply f = box_apply2 (fun f -> f.value) f

let mbind_apply f = box_apply2 (fun f -> f.values) f

let binder_from_fun name f = unbox (bind (fun _ -> assert false) name (fun x -> box_apply f x))

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

let box_pair x y = box_apply2 (fun x y -> (x,y)) x y
let box_triple x y z = box_apply3 (fun x y z -> (x,y,z)) x y z

let box_opt = function
  | None   -> box None
  | Some b -> box_apply (fun b -> Some b) b

(* Type of a context. *)
type ctxt = int list SMap.t

(* The empty context. *)
let empty_ctxt = SMap.empty

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
  let var =
    { key = fresh_key () ; prefix; suffix = new_suffix
    ; mkfree ; bindbox = dummy_bindbox }
  in
  let mk_var x htbl = Env.get (fst (IMap.find x.key htbl)) in
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

let unbind_in ctxt mkfree b =
  let (x, ctxt) = new_var_in ctxt mkfree (binder_name b) in
  (x, subst b (free_of x), ctxt)

let unmbind_in ctxt mkfree b =
  let (x, ctxt) = new_mvar_in ctxt mkfree (mbinder_names b) in
  (x, msubst b (Array.map free_of x), ctxt)
