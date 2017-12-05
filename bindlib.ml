(****************************************************************************
 * The Bindlib Library provides an efficient representation for binders (or *
 * structures with bound variables). Names are managed in the expected way, *
 * by performing minimal renaming (an integer suffix is increased in case a *
 * change of name is required).                                             *
 *                                                                          *
 * Authors:                                                                 *
 *   - Christophe Raffalli <christophe.raffalli@univ-smb.fr>                *
 *   - Rodolphe Lepigre <rodolphe.lepigre@univ-smb.fr>                      *
 ****************************************************************************)

(** [filter_map pred f l] is equivalent to  [List.map f (List.filter pred m)],
    but it is more efficient. *)
let filter_map : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list =
  fun pred f l ->
    let rec aux acc l =
      match l with
      | []   -> List.rev acc
      | x::l -> if pred x then aux (f x::acc) l else aux acc l
    in aux [] l

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
    val create : ?next_free:int -> int -> t

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
    type t = {tab : any array; mutable next_free : int}
    let create ?(next_free=0) size =
      {tab = Array.make size (Obj.repr ()); next_free}
    let set env i e = Array.set env.tab i (Obj.repr e)
    let get i env = Obj.obj (Array.get env.tab i)
    let copy env = {tab = Array.copy env.tab; next_free = env.next_free}
    let get_next_free env = env.next_free
    let set_next_free env n = env.next_free <- n
  end

(** In the internals, variables are identified by a unique [int] key. Closures
    are then formed by mapping free variables in an [Env.t]. The [varpos] type
    associates, to each variable, its index in the [Env.t] and an [int] suffix
    (used while renaming in capture-avoiding substitution). The boolean  tells
    if this variable has been substituted. *)
type varinf = { index: int; suffix: int; subst: bool }
type varpos = varinf IMap.t

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
type (+'a) bindbox =
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

(** [uid_of x] returns a unique identifier of the given variable. *)
let uid_of : 'a var -> int =
  fun x -> x.key

(** [prefix_of x] returns the [string] prefix of the given variable. *)
let prefix_of : 'a var -> string =
  fun x -> x.prefix

(** [suffix_of x] returns the [int] suffix of the given variable. *)
let suffix_of : 'a var -> int =
  fun x -> x.suffix

(** [compare_vars x y] safely compares [x] and [y].  Note that it is unsafe to
    compare variables with [Pervasive.compare]. *)
let compare_vars : 'a var -> 'b var -> int =
  fun x y -> y.key - x.key

(** [eq_vars x y] safely computes the equality of [x] and [y]. Note that it is
    unsafe to compare variables with the polymorphic equality function. *)
let eq_vars : 'a var -> 'b var -> bool =
  fun x y -> x.key = y.key

(** [hash_var x] computes a hash for variable [x]. Note that this function can
    be used with the [Hashtbl] module. *)
let hash_var : 'a var -> int =
  fun x -> Hashtbl.hash (`HVar, x.key)

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
      let {index=j; suffix; subst} = IMap.find var.key vp in
      tab.(i) <- j; (IMap.add var.key {index=i; suffix; subst} htbl, i+1)
    in
    let (new_vp,_) = List.fold_left f (IMap.empty,0) vs in
    fun env ->
      let new_env = Env.create ~next_free:size (size + n) in
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

(** [occur x b] tells whether variable [x] occurs in the [bindbox] [b]. *)
let occur : 'a var -> 'b bindbox -> bool = fun v b ->
  match b with
  | Box(_)      -> false
  | Env(vs,_,_) -> List.exists (eq_vars v) vs

(** [is_closed b] checks whether the [bindbox] [b] is closed. *)
let is_closed : 'a bindbox -> bool = fun b ->
  match b with Box(_) -> true | _ -> false

(** [is_substituted b] checks whether the [bindbox] [b] was substituted. *)
let is_substituted : (bool -> 'a) bindbox -> 'a bindbox = fun b ->
  match b with Box(f) -> Box(f false)
             | Env(vs, na, ta) ->
                let ta = fun vs ->
                  let subst = IMap.exists (fun _ i -> i.subst) vs in
                  let cla = ta vs in
                  (fun env -> cla env subst)
                in
                Env(vs, na, ta)

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

(** [unbox b] takes out the element that is in the [bindbox] b. In particular,
    if some variable have not been bound, they their [mkfree] filed is used to
    make them into elements of the expected type.  The [unbox] function should
    not be called until the construction of a term is finished (i.e., until no
    more variables need to be bound). *)
let unbox : 'a bindbox -> 'a = fun b ->
  match b with
  | Box(t)       -> t
  | Env(vs,nb,t) ->
      let nbvs = List.length vs in
      let env = Env.create ~next_free:nbvs (nbvs + nb) in
      let cur = ref 0 in
      let fn vp x =
        let i = !cur in incr cur;
        Env.set env i (x.mkfree x);
        IMap.add x.key {index=i; suffix=x.suffix; subst=false} vp
      in
      t (List.fold_left fn IMap.empty vs) env

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

(** [dummy_bindbox] can be used in uninitialised structures (e.g., arrays). If
    [unbox] is called on a data structure containing [dummy_bindbox], then the
    exception [Failure "Invalid use of dummy_bindbox"] is raised. *)
let dummy_bindbox : 'a bindbox =
  let fail _ = failwith "Invalid use of dummy_bindbox" in
  Env([], 0, fail)

(** This is safe as we can not go in the opposite direction *)
let to_any : 'a var -> any var = Obj.magic

(** [build_new_var key prefix suffix mkfree] initialises a new [var] structure
    with the given data, and updates the [bindbox] field accordingly. *)
let build_new_var : int -> string -> int -> ('a var -> 'a) -> 'a var =
  fun key prefix suffix mkfree ->
    let bindbox = Env([], 0, fun _ -> assert false) in
    let x = {key; prefix; suffix; mkfree; bindbox} in
    let mk_var vp = Env.get (IMap.find key vp).index in
    x.bindbox <- Env([to_any x], 0, mk_var); x

(** [new_var mkfree name] create a new free variable using a wrapping function
    [mkfree] and a default [name]. *)
let new_var : ('a var -> 'a) -> string -> 'a var =
  fun mkfree name ->
    let (prefix, suffix) = split_name name in
    let key = incr counter; !counter in
    build_new_var key prefix suffix mkfree

(** [new_mvar mkfree names] creates an array of new free variables in the same
    way as [new_var] does. *)
let new_mvar : ('a var -> 'a) -> string array -> 'a mvar =
  fun mkfree names -> Array.map (fun n -> new_var mkfree n) names

(** [copy_var x name mkfree] makes a copy of variable [x],  with a potentially
    different name and syntactic wrapper. However, the copy is treated exactly
    as the original in terms of binding and substitution. *)
let copy_var : 'b var -> string -> ('a var -> 'a) -> 'a var =
  fun x name mkfree ->
    let (prefix, suffix) = split_name name in
    build_new_var x.key prefix suffix mkfree

(** [get_suffix vs vp x] finds a non-colliding suffix for variable [x],  given
    a list of variables with name collisions,  the [varpos] with corresponding
    suffixes (and the positioning in the environment of the variables). *)
let get_suffix : any var list -> varpos -> 'a var -> int = fun vs vp x ->
  let pred y = x.prefix = y.prefix in
  let vs = filter_map pred (fun x -> (IMap.find x.key vp).suffix) vs in
  let rec search suffix vs =
    match vs with
    | x::vs when x < suffix -> search suffix vs
    | x::vs when x = suffix -> search (suffix+1) vs
    | _                     -> suffix
  in
  search x.suffix (List.sort (-) vs)

(** [build_binder x rank bind value] constructs a binder with the given values
    (the variable [x] is used to obtain the name of the bound variable). *)
let build_binder : 'a var -> int -> bool -> ('b -> 'c) -> ('b,'c) binder =
  fun x rank bind value ->
    let name = merge_name x.prefix x.suffix in
    {name; rank; bind; value}

(** [bind_var x b] produces a [binder] (in a [bindbox]) by binding [x] in [b].
    This is one of the main [Bindlib] functions. *)
let bind_var : 'a var -> 'b bindbox -> ('a, 'b) binder bindbox = fun x b ->
  match b with
  | Box(t)      -> Box(build_binder x 0 false (fun _ -> t))
  | Env(vs,n,t) ->
      try
        match vs with
        | [y] ->
            if x.key <> y.key then raise Not_found;
            (* The variable to bind is the last one. *)
            let t = t (IMap.singleton x.key
                                      {index=0; suffix=x.suffix; subst=true})
            in
            let value arg =
              let v = Env.create ~next_free:1 (n+1) in
              Env.set v 0 arg; t v
            in
            Box(build_binder x 0 true value)
        | _   ->
            let vs = remove x vs in
            (* General case. *)
            let cl vp =
              let x = {x with suffix = get_suffix vs vp x} in
              let rank = List.length vs in
              let t = t (IMap.add x.key
                                  {index=rank; suffix=x.suffix; subst=true} vp)
              in
              fun v ->
                let value arg =
                  let next = Env.get_next_free v in
                  if next = rank then
                    begin
                      Env.set_next_free v (next + 1);
                      Env.set v next arg; t v
                    end
                  else
                    begin
                      let v = Env.copy v in
                      Env.set_next_free v (rank+1);
                      for i = rank+1 to next-1 do Env.set v i 0 done;
                      Env.set v rank arg; t v
                    end
                in build_binder x rank true value
            in
            Env(vs, n+1, cl)
      with Not_found ->
        (* The variable does not occur. *)
        let value vp =
          let x = {x with suffix = get_suffix vs vp x} in
          let t = t vp in
          let rank = List.length vs in
          fun v -> build_binder x rank false (fun _ -> t v)
        in Env(vs, n, value)

(** [bind mkfree name f] transforms the function [f] into a binder. Everything
    happens in a bindbox, and [mkfree] and [name] are required to create a new
    variable to call [bind_var]. *)
let bind : ('a var -> 'a) -> string -> ('a bindbox -> 'b bindbox)
    -> ('a,'b) binder bindbox = fun mkfree name f ->
  let x = new_var mkfree name in
  bind_var x (f x.bindbox)

(** [vbind mkfree name f] is similar to [bind], but the domain of the function
    taken as input has type ['a var]. It also relies on [bind_var]. *)
let vbind : ('a var -> 'a) -> string -> ('a var -> 'b bindbox)
    -> ('a,'b) binder bindbox = fun mkfree name f ->
  let x = new_var mkfree name in
  bind_var x (f x)

(** [check_arity xs args] matches the size of [xs] and [args],  and raises the
    [Invalid_argument "Bad arity in msubst"] exception in case of failure. The
    error can only be triggered at runtime (in [msubst]). *)
let check_arity : 'a mvar -> 'a array -> unit = fun xs args ->
  if Array.(length xs <> length args) then invalid_arg "Bad arity in msubst"

(** [bind_mvar xs b] produces a [mbinder] (in a [bindbox]) by binding [xs]  in
    [b], in a similar way as [bind_var] does for single variables. *)
let bind_mvar : 'a mvar -> 'b bindbox -> ('a,'b) mbinder bindbox = fun xs b ->
  match b with
  | Box(t)      ->
      let values args = check_arity xs args; t in
      let binds = Array.map (fun _ -> false) xs in
      let names = Array.map name_of xs in
      Box({names; ranks = 0; binds; values})
  | Env(vs,n,t) ->
      let keys = Array.map (fun _ -> 0) xs in
      let vss = Array.map (fun _ -> vs) xs in
      let (vs, m) =
        let vs = ref vs in let m = ref n in
        for i = Array.length xs - 1 downto 0 do
          let v = xs.(i) in
          begin
            try vs := remove v !vs; incr m; keys.(i) <- v.key
            with Not_found -> keys.(i) <- -1
          end;
          vss.(i) <- !vs (*NOTE: store each vs, for good renaming *)
        done; (!vs, !m)
      in
      if vs = [] then (* All the free variables become bound. *)
        let names = Array.map (fun _ -> "") xs in
        let cur_pos = ref 0 in
        let vp = ref IMap.empty in
        let f i key =
          let suffix = get_suffix vss.(i) !vp xs.(i) in
          names.(i) <- merge_name xs.(i).prefix suffix;
          if key >= 0 then
            begin
              vp := IMap.add key {index= !cur_pos; suffix; subst=true} !vp;
              incr cur_pos; true
            end
          else false
        in
        let binds = Array.mapi f keys in
        let t = t !vp in
        let values args =
          check_arity xs args;
          let v = Env.create m in
          let pos = ref 0 in
          for i = 0 to Array.length xs - 1 do
            if binds.(i) then begin
              Env.set v !pos args.(i);
              incr pos;
            end
          done;
          Env.set_next_free v !pos;
          t v
        in
        Box({names; binds; ranks = 0; values})
      else if m = n then (* None of the variables occur. *)
        let cl vp =
          let ranks = List.length vs in
          let binds = Array.map (fun _ -> false) xs in
          let fn x = merge_name x.prefix (get_suffix vs vp x) in
          let names = Array.map fn xs in
          let t = t vp in
          fun v ->
            let values args = check_arity xs args; t v in
            {names; ranks; binds; values}
        in Env(vs, n, cl)
      else (* General case. *)
        let cl vp =
          let names = Array.map (fun _ -> "") xs in
          let ranks = List.length vs in
          let cur_pos = ref ranks in
          let vp = ref vp in
          let f i key =
            let suffix = get_suffix vss.(i) !vp xs.(i) in
            names.(i) <- merge_name xs.(i).prefix suffix;
            if key >= 0 then
              (vp := IMap.add key {index= !cur_pos;suffix; subst=true} !vp;
               incr cur_pos; true)
            else false
          in
          let binds = Array.mapi f keys in
          let t = t !vp in
          fun v ->
            let values args =
              check_arity xs args;
              let next = Env.get_next_free v in
              let cur_pos = ref ranks in
              if next = ranks then
                begin
                  for i = 0 to Array.length xs - 1 do
                    if binds.(i) then begin
                      Env.set v !cur_pos args.(i);
                      incr cur_pos;
                    end
                  done;
                  Env.set_next_free v !cur_pos;
                  t v
                end
              else
                begin
                  let v = Env.copy v in
                  for i = 0 to Array.length xs - 1 do
                    if binds.(i) then begin
                      Env.set v !cur_pos args.(i);
                      incr cur_pos;
                    end
                  done;
                  Env.set_next_free v !cur_pos;
                  for i = !cur_pos to next - 1 do Env.set v i 0 done;
                  t v
                end
            in {names; ranks; binds; values}
        in Env(vs, m, cl)

(** [mbind mkfree names f] builds a [mbinder] from the function [f]. It relies
    on [bind_mvar] to do so, and thus it require [mkfree] and [names]. *)
let mbind : ('a var -> 'a) -> string array -> ('a bindbox array -> 'b bindbox)
    -> ('a,'b) mbinder bindbox = fun mkfree names f ->
  let vs = new_mvar mkfree names in
  bind_mvar vs (f (Array.map box_of_var vs))

(** [mvbind mkfree names f] builds a [mbinder] from the function [f], like the
    [mbind] function do (up to the type of the function [f]). *)
let mvbind : ('a var -> 'a) -> string array -> ('a mvar -> 'b bindbox)
    -> ('a,'b) mbinder bindbox = fun mkfree names f ->
  let vs = new_mvar mkfree names in
  bind_mvar vs (f vs)

(** [unbind mkfree b] breaks the [binder] [b] into a variable and a body.  The
    [mkfree] function is required since it is necessary to create a  variable.
    The name of this variable is based on that of the binder. *)
let unbind : ('a var -> 'a) -> ('a,'b) binder -> 'a var * 'b =
  fun mkfree b ->
    let x = new_var mkfree (binder_name b) in
    (x, subst b (mkfree x))

(** [unbind2 mkfree f g] is similar to [unbind mkfree f], but substitutes both
    [f] and [g] using the same fresh variable. *)
let unbind2 : ('a var -> 'a) -> ('a,'b) binder -> ('a,'c) binder
    -> 'a var * 'b * 'c =
  fun mkfree b1 b2 ->
    let x = new_var mkfree (binder_name b1) in
    let v = mkfree x in
    (x, subst b1 v, subst b2 v)

(** Short name for the type of an equality function. *)
type 'a eq = 'a -> 'a -> bool

(** [eq_binder eq f g] tests the equality between [f] and [g]. The binders
    are first substituted with the same fresh variable, and [eq] is called
    on the resulting terms. *)
let eq_binder : ('a var -> 'a) -> 'b eq -> ('a,'b) binder eq =
  fun mkfree eq f g -> f == g ||
    let (x,t,u) = unbind2 mkfree f g in eq t u

(** [unmbind mkfree b] breaks the [mbinder] [b] into an array of variables and
    a body. It is required to provide a [mkfree] function since [unmbind]  has
    to create new variables. Their names are besed on the bound variables. *)
let unmbind : ('a var -> 'a) -> ('a,'b) mbinder -> 'a mvar * 'b =
  fun mkfree b ->
    let x = new_mvar mkfree (mbinder_names b) in
    (x, msubst b (Array.map mkfree x))

(** [fixpoint b] builds a binder fixpoint (advance feature). *)
let fixpoint : (('a,'b) binder, ('a,'b) binder) binder bindbox
    -> ('a, 'b) binder bindbox = fun b ->
  match b with
  | Box(t)      -> let rec fix t = t (fix t) in Box(fix t.value)
  | Env(vs,n,t) ->
     let cl vp =
       let t = t vp in
       let rec fix env = (t env).value (fix env) in fix
     in Env(vs, n, cl)

(** [bind_apply b arg] substitute a [binder] in the [bindbox] type. *)
let bind_apply : ('a,'b) binder bindbox -> 'a bindbox -> 'b bindbox =
  fun b arg -> box_apply2 subst b arg

(** [mbind_apply b args] substitute a [mbinder] in the [bindbox] type. *)
let mbind_apply : ('a,'b) mbinder bindbox -> 'a array bindbox -> 'b bindbox =
  fun b args -> box_apply2 msubst b args

(** [binder_from_fun name f] builds a [binder] from the function [f] using the
    variable name [name]. This function is very unsafe. Use farefully. *)
let binder_from_fun : string -> ('a -> 'b) -> ('a,'b) binder =
  fun name f ->
    unbox (bind (fun _ -> assert false) name (box_apply f))

(** [mbinder_from_fun names f] builds a [mbinder] from the function [f]  using
    the variable names [names]. This function is also very unsafe. *)
let mbinder_from_fun : string array -> ('a array -> 'b) -> ('a,'b) mbinder =
  fun names f ->
    let fn xs = box_apply f (box_array xs) in
    unbox (mbind (fun _ -> assert false) names fn)

(** Representation of a context, or a list of reserved names. *)
type ctxt = int list SMap.t

(** [empty_ctxt] is the empty context. *)
let empty_ctxt = SMap.empty

(** [new_var_in ctxt mkfree name] is similar to [new_var mkfree name], but the
    variable names is chosen not to collide with the context [ctxt]. Note that
    the context that is returned contains the new variable name. *)
let new_var_in : ctxt -> ('a var -> 'a) -> string -> 'a var * ctxt =
  let get_suffix name suffix ctxt =
    let rec search acc suf l =
      match l with
      | []                -> (suf, List.rev_append acc [suf])
      | x::_ when x > suf -> (suf, List.rev_append acc (suf::l))
      | x::l when x = suf -> search (x::acc) (suf+1) l
      | x::l (*x < suf*)  -> search (x::acc) suf l
    in
    try
      let (suffix, l) = search [] suffix (SMap.find name ctxt) in
      (suffix, SMap.add name l ctxt)
    with Not_found -> (suffix, SMap.add name [suffix] ctxt)
  in
  fun ctxt mkfree name ->
    let x = new_var mkfree name in
    let (suffix, ctxt) = get_suffix x.prefix x.suffix ctxt in
    ({x with suffix}, ctxt)

(** [new_mvar_in ctxt mkfree names] is similar to [new_mvar mkfree names], but
    it handles the context (see [new_var_in]). *)
let new_mvar_in : ctxt -> ('a var -> 'a) -> string array -> 'a mvar * ctxt =
  fun ctxt mkfree names ->
    let ctxt = ref ctxt in
    let f name =
      let (v, new_ctxt) = new_var_in !ctxt mkfree name in
      ctxt := new_ctxt; v
    in
    let vs = Array.map f names in
    (vs, !ctxt)

(** [bind_in ctxt mkfree name f] is like [bind mkfree name f],  but it handles
    the context. *)
let bind_in : ctxt -> ('a var -> 'a) -> string
    -> ('a bindbox -> ctxt -> 'b bindbox) -> ('a,'b) binder bindbox =
  fun ctxt mkfree name f ->
    let (v, ctxt) = new_var_in ctxt mkfree name in
    bind_var v (f v.bindbox ctxt)

(** [mbind_in ctxt mkfree names f] is similar to  [mbind mkfree names f],  but
    it handles the context. *)
let mbind_in : ctxt -> ('a var -> 'a) -> string array
    -> ('a bindbox array -> ctxt -> 'b bindbox) -> ('a,'b) mbinder bindbox =
  fun ctxt mkfree names fpt ->
    let (vs, ctxt) = new_mvar_in ctxt mkfree names in
    let args = Array.map box_of_var vs in
    bind_mvar vs (fpt args ctxt)

(** [unbind_in ctxt mkfree b] is similar to [unbind mkfree b],  but it handles
    the context (see [new_mvar_in]). *)
let unbind_in : ctxt -> ('a var -> 'a) -> ('a,'b) binder
    -> 'a var * 'b * ctxt = fun ctxt mkfree b ->
  let (x, ctxt) = new_var_in ctxt mkfree (binder_name b) in
  (x, subst b (mkfree x), ctxt)

(** [munbind_in ctxt mkfree b] is like [munbind mkfree b],  but it handles the
    context (see [new_mvar_in]). *)
let unmbind_in : ctxt -> ('a var -> 'a) -> ('a,'b) mbinder
    -> 'a mvar * 'b * ctxt = fun ctxt mkfree b ->
  let (x, ctxt) = new_mvar_in ctxt mkfree (mbinder_names b) in
  (x, msubst b (Array.map mkfree x), ctxt)
