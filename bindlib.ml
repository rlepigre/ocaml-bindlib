(****************************************************************************
 * The Bindlib Library provides datatypes to represent binders in arbitrary *
 * languages. The implementation is efficient and manages name in the expe- *
 * ted way (variables have a prefered name to which an integer  suffix  can *
 * be added to avoid capture during substitution.                           *
 *                                                                          *
 * Authors:                                                                 *
 *   - Christophe Raffalli <christophe.raffalli@univ-smb.fr>                *
 *   - Rodolphe Lepigre <rodolphe.lepigre@univ-smb.fr>                      *
 ****************************************************************************)

(** Counter for fresh symbol generation. *)
let counter : int ref = ref (-1)

(** [reset_counter ()] resets the counter. This function should only be called
    when previously generated [Bindlib] data structures cannot be accessed any
    more. *)
let reset_counter : unit -> unit = fun () -> counter := (-1)

(** Maps with [int] keys. *)
module IMap = Map.Make(
  struct
    type t = int
    let compare = (-)
  end)

(** Maps with [string] keys. *)
module SMap = Map.Make(String)

(** Type of anything. *)
type any = Obj.t

(** An environment is used to store the value of every bound variables. We use
    the [Obj] module to strore variables with potentially different types in a
    single array. However, this module is only used in a safe way. *)
module Env :
  sig
    (** Type of an environment. *)
    type t

    (** Creates an empty environment of a given size. *)
    val create : int -> t

    (** Sets the value stored at some position in the environment. *)
    val set : t -> int -> 'a -> unit

    (** Gets the value stored at some position in the environment. *)
    val get : int -> t -> 'a

    (** Make a copy of the environment. *)
    val copy : t -> t

    (** Get next free cell index. *)
    val get_next_free : t -> int

    (** Set the next free cell index. *)
    val set_next_free : t -> int -> unit
  end =
  struct
    type t = {tab : any array; mutable next : int}
    let create size = {tab = Array.make size (Obj.repr ()); next = 0}
    let set env i e = Array.set env.tab i (Obj.repr e)
    let get i env = Obj.obj (Array.get env.tab i)
    let copy env = {tab = Array.copy env.tab; next = env.next}
    let get_next_free env = env.next
    let set_next_free env n = env.next <- n
  end

(** In the internals, variables are identified by a unique [int] key. Closures
    are then formed by mapping free variables in an [Env.t]. The [varpos] type
    associates, to each variable, its index in the [Env.t] and an [int] suffix
    (used while renaming in capture-avoiding substitution). *)
type varpos = (int * int) IMap.t

(** A closure of type ['a] is represented as a function taking as input a  map
    ([varpos]) and an environment ([Env.t]). *)
type 'a closure = varpos -> Env.t -> 'a

(** [map_closure f cl] applies the function [f] under the closure [cl], making
    sure that the [varpos] is computed as soon as possible. *)
let map_closure : ('a -> 'b) -> 'a closure -> 'b closure =
  fun f cla vs -> (fun a env -> f (a env)) (cla vs)

(** [app_closure cl a] applies the argument [a] to the closure [cl]. Note that
    we make sure that the [varpos] is computed as soon as possible. *)
let app_closure : ('a -> 'b) closure -> 'a -> 'b closure =
  fun clf a vs -> (fun f env -> f env a) (clf vs)

(** [clf <*> cla] applies the function closure [clf] to the  argument  closure
    [cla]. Note that the [varpos] are computed as soon as possible.  Note also
    that the [(<*>)] operator is the "apply" of an applicative functor. *)
let (<*>) : ('a -> 'b) closure -> 'a closure -> 'b closure =
  fun clf cla vs -> (fun f a env -> f env (a env)) (clf vs) (cla vs)

(** Elements of the type ['a] with bound variables are constructed in the type
    ['a bindbox]. A free variable can only be bound under this constructor. In
    other words, and element of type ['a bindbox] corresponds to an element of
    type ['a] which free variables may be bound later. *)
type 'a bindbox =
  | Box of 'a
  (* Element of type ['a] with no free variable. *)
  | Env of any var list * int * 'a closure
  (* Element of type ['a] with free variables stored in an environment. *)

(** Note that in [Env(vs,nb,t)] we store the list [vs] of every free variables
    (stored by key), the number [nb] of bound variables having a reserved slot
    in the environment, the open term [t] itself. The term [t] should be given
    the environment as second argument, and the position of the free variables
    of [vs] in the environmenet as a first argument. *)

(** Important remark: the function of type [varpos -> Env.t -> 'a] is going to
    be used to build efficient substitutions. They are represented as closures
    waiting for an environment.  This means that the [varpos] map is used only
    once for each variable, even if the variable appears many times. *)

(** Type of a free variable of type ['a]. *)
and 'a var =
  { key             : int          (* Unique identifier.                *)
  ; prefix          : string       (* Name as a free variable (prefix). *)
  ; suffix          : int          (* Integer suffix.                   *)
  ; mkfree          : 'a var -> 'a (* Function to build a term.         *)
  ; mutable bindbox : 'a bindbox   (* Bindbox containing the variable.  *) }

(** Type of an array of variables of type ['a]. *)
type 'a mvar = 'a var array

(** [merge_name prefux suffux] builds a variable name using a [string]  prefix
    and an [int] suffix. *)
let merge_name : string -> int -> string =
  fun pr sf -> if sf >= 0 then pr ^ (string_of_int sf) else pr

(** [split_name s] splits [s] into a [string] prefix and an [int] suffix. Note
    that we have [split "xyz" = ("xyz", (-1))], [split "xyz12" = ("xyz", 12)], 
    or [split "12" = ("", 12)]. In other words, we take the longest suffix. *)
let split_name : string -> string * int = fun name ->
  let is_digit c = '0' <= c && c <= '9' in
  let len = String.length name in
  let last_digit = ref len in
  while !last_digit > 0 && is_digit name.[!last_digit - 1] do
    decr last_digit
  done;
  if !last_digit = len then (name, (-1)) else
    let pref = String.sub name 0 !last_digit in
    let suff = String.sub name !last_digit (len - !last_digit) in
    (pref, int_of_string suff)

(** [name_of x] computes the full name of the given variable. *)
let name_of : 'a var -> string =
  fun x -> merge_name x.prefix x.suffix

(** [prefix_of x] returns the [string] prefix of the given variable. *)
let prefix_of : 'a var -> string =
  fun x -> x.prefix

(** [suffix_of x] returns the [int] suffix of the given variable. *)
let suffix_of : 'a var -> int =
  fun x -> x.suffix

(** [compare_vars x y] safely compare [x] and [y]. Note that it is not safe to
    compare variables with [Pervasive.compare]. *)
let compare_vars : 'a var -> 'b var -> int =
  fun x y -> y.key - x.key

(** [eq_vars x y] safely compute the equality of [x] and [y]. Again, it is not
    save to compare variables with the polymorphic equality function. *)
let eq_vars : 'a var -> 'b var -> bool =
  fun x y -> x.key = y.key

(** [hash_var x] computes a hash for variable [x]. Note that this function can
    be used with the [Hashtbl] module. *)
let hash_var : 'a var -> int =
  fun x -> Hashtbl.hash (`HVar, x.key)

(** [free_of x] wraps variable [x] into an element of its type. Note that this
    function relies on a function of type ['a var -> 'a], provided by the user
    at the creation of a variable. *)
let free_of : 'a var -> 'a =
  fun x -> x.mkfree x

(** [box_of_var x] builds a [bindbox] from variable [x]. *)
let box_of_var : 'a var -> 'a bindbox =
  fun x -> x.bindbox

(** [merge_uniq l1 l2] merges two sorted lists of variables that must not have
    any repetitions. The produced list does not have repetition eigher. *)
let merge_uniq : any var list -> any var list -> any var list =
  let rec merge_uniq acc l1 l2 =
    match (l1, l2) with
    | ([]   , _    ) -> List.rev_append acc l2
    | (_    , []   ) -> List.rev_append acc l1
    | (x::xs, y::ys) when x.key = y.key -> merge_uniq (x::acc) xs ys
    | (x::xs, y::ys) when x.key < y.key -> merge_uniq (x::acc) xs l2
    | (x::xs, y::ys) (*x.key > y.key*)  -> merge_uniq (y::acc) l1 ys
  in merge_uniq []

(** [remove x l] removes variable [x] from the list [l]. If [x] is not in [l],
    then the exception [Not_found] is raised. *)
let remove : 'a var -> any var list -> any var list = fun {key} ->
  let rec remove acc = function
    | v::l when v.key < key -> remove (v::acc) l
    | v::l when v.key = key -> List.rev_append acc l
    | _                     -> raise Not_found
  in remove []

(** [minimize vs n cl] builds a minimal closure that is equivalent to [cl] and
    only contains variables of [vs]. Note that [n] extra slots are reserved in
    the environment. *)
let minimize : any var list -> int -> 'a closure -> 'a closure = fun vs n t ->
  if n = 0 then t else
  fun vp ->
    let size = List.length vs in
    let tab = Array.make size 0 in
    let f (htbl, i) var =
      let (j, suf) = IMap.find var.key vp in
      tab.(i) <- j; (IMap.add var.key (i, suf) htbl, i+1)
    in
    let (new_vp,_) = List.fold_left f (IMap.empty,0) vs in
    fun env ->
      let new_env = Env.create (size + n) in
      Env.set_next_free new_env size;
      for i = 0 to size - 1 do
        Env.set new_env i (Env.get tab.(i) env)
      done;
      t new_vp new_env

(** [box e] injects the element [e] in the [bindbox] type, without considering
    its structure. In particular, it will not be possible to bind variables in
    [e] at all. *)
let box : 'a -> 'a bindbox = fun t -> Box (t)

(** [apply_box f a] performs application inside the [bindbox] type constructor
    (it corresponds to "fmap" in the applicative functor sense). It allows the
    application of a function (with free variables) to an argument  (with free
    variables), to produce a result with free variables. Note that, during the
    construction of the application, we use the [select] function to build the
    minimal closure when both parts of the application have free variables. *)
let apply_box : ('a -> 'b) bindbox -> 'a bindbox -> 'b bindbox = fun f a ->
  match (f, a) with
  | (Box(f)       , Box(a)       ) -> Box(f a)
  | (Box(f)       , Env(va,na,ta)) -> Env(va, na, map_closure f ta)
  | (Env(vf,nf,tf), Box(a)       ) -> Env(vf, nf, app_closure tf a)
  | (Env(vf,nf,tf), Env(va,na,ta)) ->
      Env(merge_uniq vf va, 0, minimize vf nf tf <*> minimize va na ta)

(** [occur v] tells whether variable [v] occurs in the [bindbox] [b]. *)
let occur : 'a var -> 'b bindbox -> bool = fun v b ->
  match b with
  | Box(_)      -> false
  | Env(vs,_,_) -> List.exists (eq_vars v) vs

(** [is_closed b] checks whether the [bindbox] [b] is closed. *)
let is_closed : 'a bindbox -> bool = fun b ->
  match b with Box(_) -> true | _ -> false

(** [box_apply f a] maps the function [f] into the [bindbox] [a]. Note that it
    is equivalent to [apply_box (box f) a], but it is more efficient. *)
let box_apply : ('a -> 'b) -> 'a bindbox -> 'b bindbox = fun f a ->
  match a with
  | Box(a)        -> Box(f a)
  | Env(vs,na,ta) -> Env(vs, na, map_closure f ta)

(** Functions similar to [box_apply]. *)
let box_apply2 f ta tb       = apply_box (box_apply f ta) tb
let box_apply3 f ta tb tc    = apply_box (box_apply2 f ta tb) tc
let box_apply4 f ta tb tc td = apply_box (box_apply3 f ta tb tc) td

(** Boxing functions for pairs and triples. *)
let box_pair x y = box_apply2 (fun x y -> (x,y)) x y
let box_triple x y z = box_apply3 (fun x y z -> (x,y,z)) x y z

(** Boxing function for the [option] type. *)
let box_opt o =
  match o with
  | None    -> box None
  | Some(e) -> box_apply (fun e -> Some(e)) e

(** Type of the data collected by the [gather_data] utility function. *)
type data = bool * any var list * int

(** [gather_data acc b] collects some data about a [bindbox] [b]. The gathered
    information contains a boolean indicating whether the considered [bindbox]
    is a [Box], the list of all the variables and the number of slots reserved
    in the environment. This data is accumulated into [acc]. *)
let gather_data (only_box, vs_acc, n_acc) b =
  match b with
  | Box(e)      -> (only_box, vs_acc              , n_acc      )
  | Env(vs,n,_) -> (false   , merge_uniq vs_acc vs, max n_acc n)

(** [no_data] is in some sense the neutral element of [gather_data]. *)
let no_data : data = (true, [], 0)

(** [bindbox_to_closure b] extracts a [closure] from a [bindbox]. Note that in
    the process, the variables and the reserved slots are lost. It is intended
    to be used in conjunction with [gather_data]. *)
let bindbox_to_closure : 'a bindbox -> 'a closure = fun b ->
  match b with
  | Box(t)      -> fun _ _ -> t
  | Env(vs,n,t) -> minimize vs n t

(** Type of a module equipped with a [map] function. *)
module type Map =
  sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

(** Functorial interface used to build lifting functions (i.e., functions that
    permute the [bindbox] type with another type constructor). *)
module Lift(M : Map) =
  struct
    let lift_box : 'a bindbox M.t -> 'a M.t bindbox =
      fun m ->
        let data = ref no_data in
        let fn b = data := gather_data !data b; bindbox_to_closure b in
        let m = M.map fn m in
        let aux vp =
          let m = M.map (fun o -> o vp) m in
          fun env -> M.map (fun o -> o env) m
        in
        match !data with
        | (true, _ , _) -> Box(aux IMap.empty (Env.create 0))
        | (_   , vs, n) -> Env(vs, n, minimize vs n aux)
  end

(** Type of a module equipped with a "binary" [map] function. *)
module type Map2 =
  sig
    type ('a, 'b) t
    val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
  end

(** Similar to the [Lift] functor, but handles "binary" [map] functions. *)
module Lift2(M : Map2) =
  struct
    let lift_box : ('a bindbox, 'b bindbox) M.t -> ('a,'b) M.t bindbox =
      fun m ->
        let data = ref no_data in
        let fn b = data := gather_data !data b; bindbox_to_closure b in
        let m = M.map fn fn m in
        let aux vp =
          let m = M.map (fun o -> o vp) (fun o -> o vp) m in
          fun env -> M.map (fun o -> o env) (fun o -> o env) m
        in
        match !data with
        | (true, _ , _) -> Box(aux IMap.empty (Env.create 0))
        | (_   , vs, n) -> Env(vs, n, minimize vs n aux)
  end

(** Lifting function for the [list] type. *)
module Lift_list = Lift(
  struct
    type 'a t = 'a list
    let map = List.map
  end)
let box_list = Lift_list.lift_box

(** Alternative lifting function for the [list] type. Note that the input list
    is reversed in the process, which makes [box_rev_list] more efficient that
    [box_list]. *)
module Lift_rev_list = Lift(
  struct
    type 'a t = 'a list
    let map = List.rev_map
  end)
let box_rev_list = Lift_rev_list.lift_box

(** Lifting function for the [array] type. *)
module Lift_array = Lift(
  struct
    type 'a t = 'a array
    let map = Array.map
  end)
let box_array = Lift_array.lift_box

(** The representation of a [binder],  which is an element of type ['b] with a
    bound variable of type ['a]. *)
type ('a,'b) binder =
  { name  : string   (** Name of the bound variable.                *)
  ; bind  : bool     (** Indicates whether the variable occurs.     *)
  ; rank  : int      (** Number of remaining free variables (>= 0). *)
  ; value : 'a -> 'b (** Substitution function.                     *) }

(** [binder_name] returns the name of the variable bound by the [binder]. *)
let binder_name : ('a,'b) binder -> string = fun b -> b.name

(** [subst b v] substitutes the variable bound by [b], using [v]. *)
let subst : ('a,'b) binder -> 'a -> 'b = fun b x -> b.value x

(** [binder_occur b] tests whether the bound variable occurs in [b]. *)
let binder_occur : ('a,'b) binder -> bool = fun b -> b.bind

(** [binder_constant b] tests whether the [binder] [b] is constant (i.e.,  its
    bound variable does not occur). *)
let binder_constant : ('a,'b) binder -> bool = fun b -> not b.bind

(** [binder_closed b] test whether the [binder] [b] is closed (i.e.,  does not
    contain any free variable). *)
let binder_closed : ('a,'b) binder -> bool = fun b -> b.rank = 0

(** [binder_rank b] gives the number of free variables contained in [b]. *)
let binder_rank : ('a,'b) binder -> int = fun b -> b.rank

(** [binder_compose_left f b] precomposes the binder [b] with the function [f]
    without changing anything at the binding structure. *)
let binder_compose_left  : ('a -> 'b) -> ('b,'c) binder -> ('a,'c) binder =
  fun f b -> { b with value = fun x -> b.value (f x) }

(** [binder_compose_rigth b f] postcomposes the binder [b] with  the  function
    [f] without changing anything at the binding structure. *)
let binder_compose_right : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder =
  fun b f -> { b with value = fun x -> f (b.value x) }

(** The representation of a multiple binder,  which binds several variables at
    once. It corresponds to an expression of type ['b] with bound variables of
    type ['a]. *)
type ('a,'b) mbinder =
  { names  : string array   (** Names of the bound variables.          *)
  ; binds  : bool array     (** Indicates whether the variables occur. *)
  ; ranks  : int            (** Number of remaining free variables.    *)
  ; values : 'a array -> 'b (** Substitution function.                 *) }

(** [mbinder_arity b] gives the arity of the [mbinder]. *)
let mbinder_arity : ('a,'b) mbinder -> int = fun mb -> Array.length mb.names

(** [mbinder_names b] return the array of the names of the variables bound  by
    the [mbinder] [b]. *)
let mbinder_names : ('a,'b) mbinder -> string array = fun mb -> mb.names

(** [msubst b vs] substitutes the variables bound by [b], using the array [vs]
    (which size should correspond to [mbinder_arity b]). *)
let msubst : ('a,'b) mbinder -> 'a array -> 'b = fun mb xs -> mb.values xs

(** [mbinder_occurs b] returns an array of [bool] indicating if the  variables
    that are bound occur (i.e., are used). *)
let mbinder_occurs : ('a,'b) mbinder -> bool array = fun mb -> mb.binds

(** [mbinder_constant b] indicates whether the [mbinder] [b] is constant. This
    means that none of its variables are used. *)
let mbinder_constant : ('a,'b) mbinder -> bool =
  fun mb -> Array.fold_left (||) false mb.binds

(** [mbinder_closed b] indicates whether [b] is closed. *)
let mbinder_closed : ('a,'b) mbinder -> bool = fun mb -> mb.ranks = 0

(* [mbinder_rank b] gives the number of free variables contained in [b]. *)
let mbinder_rank : ('a,'b) mbinder -> int = fun mb -> mb.ranks


(* FIXME FIXME FIXME chantier FIXME FIXME FIXME *)


(* Dummy bindbox to be used prior to initialisation. *)
let dummy_bindbox : 'a bindbox =
  Env([], 0, fun _ -> failwith "Invalid use of dummy_bindbox")

(* Function for building a variable structure with a fresh key. *)
let build_new_var name mkfree bindbox =
  let (prefix, suffix) = split_name name in
  let key = incr counter; !counter in
  {key; prefix; suffix; mkfree; bindbox}

let update_var_bindbox : 'a var -> unit =
  let mk_var x htbl = Env.get (fst (IMap.find x.key htbl)) in
  fun x -> x.bindbox <- Env([Obj.magic x], 0, mk_var x)

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
    update_var_bindbox r; r

(* Find a non-colliding suffix given a list of variables keys with name
collisions, the hashtable containing the corresponding suffixes (and the
positioning in the environment of the variables), and a prefered suffix. *)
let get_suffix : 'a. any var list -> varpos -> 'a var -> int =
  let filter_map cond fn l =
    let rec aux acc = function
      | []   -> List.rev acc
      | x::l -> if cond x then aux (fn x::acc) l else aux acc l
    in aux [] l
  in
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


(* Get out of the [bindbox] structure and construct an actual term. When the
term is closed, it is straight-forward, but when it is open, free variables
need to be made free in the syntax using the [mkfree] field. *)
let unbox : 'a bindbox -> 'a = function
  | Box(t) -> t
  | Env(vt,nbt,t) ->
      let next = List.length vt in
      let esize = next + nbt in
      let env = Env.create esize in
      Env.set_next_free env next;
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
  let v = Env.create esize in Env.set_next_free v 1;
  Env.set v 0 arg;
  pt v

let mk_first_bind x esize pt =
  let htbl = IMap.add x.key (0,x.suffix) IMap.empty in
  let pt = pt htbl in
  { name = merge_name x.prefix x.suffix; rank = 0; bind = true
  ; value = value_first_bind esize pt }

(* Build a normal binder (the default case) *)
let value_bind pt pos v arg =
  let next = Env.get_next_free v in
  if next = pos then
    (Env.set v next arg; Env.set_next_free v (next + 1); pt v)
  else
    (let v = Env.copy v in
     Env.set_next_free v (pos + 1); Env.set v pos arg;
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
  | Box t ->
     Box {name = merge_name x.prefix x.suffix;
             rank = 0; bind = false; value = fun _ -> t}
  | Env(vs,nb,t) ->
     try
       match vs with
       | [y] -> if x.key <> y.key then raise Not_found;
                Box (mk_first_bind x (nb + 1) t)
       | _   -> let vt = remove x vs in
                let pos = List.length vt in
                Env(vt, nb + 1, mk_bind vt x pos t)
    with Not_found ->
      let rank = List.length vs in
      Env(vs, nb, mk_mute_bind vs x rank t)

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
  let next = Env.get_next_free v in
  let cur_pos = ref pos in
  if next = pos then begin
    for i = 0 to arity - 1 do
      if binds.(i) then begin
        Env.set v !cur_pos args.(i);
        incr cur_pos;
      end
    done;
    Env.set_next_free v !cur_pos;
    pt v
  end else begin
    let v = Env.copy v in
    for i = 0 to arity - 1 do
      if binds.(i) then begin
        Env.set v !cur_pos args.(i);
        incr cur_pos;
      end
    done;
    Env.set_next_free v !cur_pos;
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
  Env.set_next_free v !cur_pos;
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
  | Box t ->
      let values args =
        if Array.length vs <> Array.length args then
          raise (Invalid_argument "bad arity in msubst");
        t
      in
      let binds = Array.map (fun _ -> false) vs in
      let names = Array.map (fun x ->
                      merge_name x.prefix x.suffix) vs in
      Box {names; ranks = 0; binds ; values}
  | Env(vt,nbt,t) ->
      let vt = ref vt in
      let nnbt = ref nbt in
      let len = Array.length vs in
      let keys = Array.make len 0 in
      for i = len - 1 downto 0 do
        let v = vs.(i) in
        try
          let ng = remove v !vt in
          incr nnbt;
          vt := ng;
          keys.(i) <- v.key
        with Not_found ->
          keys.(i) <- -1
      done;
      let vt = !vt in
      if vt = [] then
        Box(mk_first_mbind [] vs keys !nnbt t)
      else if !nnbt = nbt then
        Env(vt,nbt,mk_mute_mbind vt vs t)
      else
        Env(vt,!nnbt,mk_mbind vt vs keys t)

(* Take a function of type ['a bindbox array -> 'b bindbox] and builds the
corresponing multi-binder. *)
let mbind mkfree names f =
  let vs = new_mvar mkfree names in
  let args = Array.map box_of_var vs in
  bind_mvar vs (f args)

let mvbind mkfree names f =
  let vs = new_mvar mkfree names in
  bind_mvar vs (f vs)

let bind_apply f = box_apply2 (fun f -> f.value) f

let mbind_apply f = box_apply2 (fun f -> f.values) f

let binder_from_fun name f = unbox (bind (fun _ -> assert false) name (fun x -> box_apply f x))

let fixpoint = function
  | Box t      -> let rec fix t =
                       t (fix t)
                     in Box(fix t.value)
  | Env(vs,nb,t) ->
     let fix t htbl =
       let t = t htbl in
       let rec fix' env = (t env).value (fix' env) in fix'
     in Env(vs, nb, fix t)



let mbinder_from_fun names f = unbox (mbind (fun _ -> assert false) names
                                            (fun xs ->
                                              let xs = box_array xs in
                                              box_apply f xs))



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
  let x = new_var mkfree name in
  let (new_suffix, ctxt) = get_suffix x.prefix x.suffix ctxt in
  ({x with suffix = new_suffix}, ctxt)

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
