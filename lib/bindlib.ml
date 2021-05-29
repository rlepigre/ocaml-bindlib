(** The [Bindlib] library provides support for free and bound variables in the
    OCaml language. The main application is the construction of abstract types
    containing a binding structure (e.g., abstract syntax trees).

    @author Christophe Raffalli
    @author Rodolphe Lepigre *)


(** [reset_counter ()] resets the counter. This function should only be called
    when previously generated [Bindlib] data structures cannot be accessed any
    more. [fresh_key ()] produces a fresh key, using the hidden counter. *)
let ((reset_counter : unit -> unit), (fresh_key : unit -> int)) =
  let c = ref (-1) in
  ((fun () -> c := -1), (fun () -> incr c; !c))

(** Maps with [int] keys. *)
module IMap = Map.Make(
  struct
    type t = int
    let compare = (-)
  end)

(** Maps with [string] keys. *)
module SMap = Map.Make(String)

(** An environment is used to store the value of every bound variables. We use
    the [Obj] module to store variables with potentially different types in  a
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

    (** [blit s t n] copies the [n] first elements of [s] to [t]. *)
    val blit : t -> t -> int -> unit

    (** Make a copy of the environment. *)
    val copy : t -> t

    (** Get next free cell index. *)
    val get_next_free : t -> int

    (** Set the next free cell index. *)
    val set_next_free : t -> int -> unit
  end =
  struct
    type t = {tab : Obj.t array; mutable next_free : int}
    let create ?(next_free=0) size =
      {tab = Array.make size (Obj.repr ()); next_free}
    let set env i e = Array.set env.tab i (Obj.repr e)
    let get i env = Obj.obj (Array.get env.tab i)
    let blit src dst len = Array.blit src.tab 0 dst.tab 0 len
    let copy env = {tab = Array.copy env.tab; next_free = env.next_free}
    let get_next_free env = env.next_free
    let set_next_free env n = env.next_free <- n
  end

(** In the internals, variables are identified by a unique [int] key. Closures
    are then formed by mapping free variables in an [Env.t]. The [varpos] type
    associates, to each variable, its index in the [Env.t]. *)
type varinf = int
type varpos = varinf IMap.t

(** A closure of type ['a] is represented as a function taking as input a  map
    ([varpos]) and an environment ([Env.t]). *)
type 'a closure = varpos -> Env.t -> 'a

(** [map_closure f cl] applies the function [f] under the closure [cl], making
    sure that the [varpos] is computed as soon as possible. *)
let map_closure_aux f a = fun env -> f (a env)
let map_closure : ('a -> 'b) -> 'a closure -> 'b closure =
  fun f cla vs -> map_closure_aux f (cla vs)

(** [app_closure cl a] applies the argument [a] to the closure [cl]. Note that
    we make sure that the [varpos] is computed as soon as possible. *)
let app_closure_aux f a = fun env -> f env a
let app_closure : ('a -> 'b) closure -> 'a -> 'b closure =
  fun clf a vs -> app_closure_aux (clf vs) a

(** [clf <*> cla] applies the function closure [clf] to the  argument  closure
    [cla]. Note that the [varpos] are computed as soon as possible.  Note also
    that the [(<*>)] operator is the "apply" of an applicative functor. *)
let apply_closure_aux f a = fun env -> f env (a env)
let (<*>) : ('a -> 'b) closure -> 'a closure -> 'b closure =
  fun clf cla vs -> apply_closure_aux (clf vs) (cla vs)

(** Elements of the type ['a] with bound variables are constructed in the type
    ['a box]. A free variable can only be bound under this constructor. Hence,
    an element of type ['a box] can be understood as an element of  type  ['a]
    whose free variables may be bound later. *)
type 'a box =
  | Box of 'a
  (* Element of type ['a] with no free variable. *)
  | Env of any_var list * int * 'a closure
  (* Element of type ['a] with free variables stored in an environment. *)

(** Note that in [Env(vs,nb,t)] we store the list [vs] of every free variables
    (stored by key), the number [nb] of bound variables having a reserved slot
    in the environment, the open term [t] itself. The term [t] should be given
    the environment as second argument, and the position of the free variables
    of [vs] in the environment as a first argument. *)

(** Important remark: the function of type [varpos -> Env.t -> 'a] is going to
    be used to build efficient substitutions. They are represented as closures
    waiting for an environment.  This means that the [varpos] map is used only
    once for each variable, even if the variable appears many times. *)

(** Type of a free variable of type ['a]. *)
and 'a var =
  { var_key    : int          (* Unique identifier.               *)
  ; var_name   : string       (* Name as a free variable.         *)
  ; var_mkfree : 'a var -> 'a (* Function to build a term.        *)
  ; var_box    : 'a box       (* Bindbox containing the variable. *) }

(** Variable of any type (using an existential). *)
and any_var = V : 'a var -> any_var (* [@@ocaml.unboxed] *)
(* FIXME the unboxed tag above is rejected due to an OCaml bug. *)

(** Type of an array of variables of type ['a]. *)
type 'a mvar = 'a var array

(** [name_of x] gives the name of the given variable. It avoid captures
    only if you are using context (type [ctxt] below). Typically, with
    bindlib, you perform renaming only at printing, not before *)
let name_of : 'a var -> string = fun x -> x.var_name

(** [names_of xs] returns names for the variables of [xs]. The same
    coment as above applies *)
let names_of : 'a mvar -> string array = fun xs -> Array.map name_of xs

(** [uid_of x] returns a unique identifier of the given variable. *)
let uid_of : 'a var -> int = fun x -> x.var_key

(** [uids_of xs] returns the unique identifiers of the variables of [xs]. *)
let uids_of : 'a mvar -> int array = fun xs -> Array.map uid_of xs

(** [compare_vars x y] safely compares [x] and [y].  Note that it is unsafe to
    compare variables with [Pervasive.compare]. *)
let compare_vars : 'a var -> 'b var -> int = fun x y -> y.var_key - x.var_key

(** [eq_vars x y] safely computes the equality of [x] and [y]. Note that it is
    unsafe to compare variables with the polymorphic equality function. *)
let eq_vars : 'a var -> 'b var -> bool = fun x y -> x.var_key = y.var_key

(** [hash_var x] computes a hash for variable [x]. Note that this function can
    be used with the [Hashtbl] module. *)
let hash_var : 'a var -> int = fun x -> Hashtbl.hash (`HVar, x.var_key)

(** [box_var x] boxes variable [a] to make it available for binding. *)
let box_var : 'a var -> 'a box = fun x -> x.var_box

(** [merge_uniq l1 l2] merges two sorted lists of variables that must not have
    any repetitions. The produced list does not have repetition either. *)
let merge_uniq : any_var list -> any_var list -> any_var list =
  let rec merge_uniq acc l1 l2 =
    match (l1, l2) with
    | ([]              , _               ) -> List.rev_append acc l2
    | (_               , []              ) -> List.rev_append acc l1
    | ((V(x) as vx)::xs, (V(y) as vy)::ys) ->
        if x.var_key = y.var_key then merge_uniq (vx::acc) xs ys else
        if x.var_key < y.var_key then merge_uniq (vx::acc) xs l2 else
        (* x.var_key > y.var_key*)    merge_uniq (vy::acc) l1 ys
  in merge_uniq []

(** [remove x l] removes variable [x] from the list [l]. If [x] is not in [l],
    then the exception [Not_found] is raised. *)
let remove : 'a var -> any_var list -> any_var list = fun {var_key ; _} ->
  let rec remove acc = function
    | (V(x) as v)::l when x.var_key < var_key -> remove (v::acc) l
    | (V(x)     )::l when x.var_key = var_key -> List.rev_append acc l
    | _                                       -> raise Not_found
  in remove []

(** [minimize vs n cl] builds a minimal closure that is equivalent to [cl] and
    only contains variables of [vs]. Note that [n] extra slots are reserved in
    the environment. *)
let minimize_aux_prefix size n t = fun env ->
  let new_env = Env.create ~next_free:size (size + n) in
  Env.blit env new_env size; t new_env
let minimize_aux tab n t = fun env ->
  let size = Array.length tab in
  let new_env = Env.create ~next_free:size (size + n) in
  Array.iteri (fun i x -> Env.set new_env i (Env.get x env)) tab;
  t new_env
let minimize : any_var list -> int -> 'a closure -> 'a closure = fun vs n t ->
  if n = 0 then t else
  fun vp ->
    let size = List.length vs in
    let tab = Array.make size 0 in
    let prefix = ref true in
    let f (new_vp, i) (V(var)) =
      let j = IMap.find var.var_key vp in
      prefix := !prefix && i = j; tab.(i) <- j;
      (IMap.add var.var_key i new_vp, i+1)
    in
    let (new_vp,_) = List.fold_left f (IMap.empty,0) vs in
    let t = t new_vp in
    if !prefix then minimize_aux_prefix size n t else minimize_aux tab n t

(** [box e] injects the element [e] in the [box] type, without considering its
    structure. It will thus be impossible to bind variables in [e]. *)
let box : 'a -> 'a box = fun t -> Box (t)

(** [apply_box f a] performs application inside the [box] type constructor (it
    corresponds to "fmap" in the applicative functor sense). It can be used to
    apply a function with free variables to an argument with free variables to
    produce a result with free variables. Note that during the construction of
    the application, we use the [select] function to build the minimal closure
    when both parts of the application have free variables. *)
let apply_box : ('a -> 'b) box -> 'a box -> 'b box = fun f a ->
  match (f, a) with
  | (Box(f)       , Box(a)       ) -> Box(f a)
  | (Box(f)       , Env(va,na,ta)) -> Env(va, na, map_closure f ta)
  | (Env(vf,nf,tf), Box(a)       ) -> Env(vf, nf, app_closure tf a)
  | (Env(vf,nf,tf), Env(va,na,ta)) ->
      Env(merge_uniq vf va, 0, minimize vf nf tf <*> minimize va na ta)

(** [occur x b] tells whether variable [x] occurs in the [box] [b]. *)
let occur : 'a var -> 'b box -> bool = fun v b ->
  match b with
  | Box(_)      -> false
  | Env(vs,_,_) -> List.exists (fun (V(x)) -> x.var_key = v.var_key) vs

(** [is_closed b] checks whether the [box] [b] is closed. *)
let is_closed : 'a box -> bool = fun b ->
  match b with Box(_) -> true | Env(_,_,_) -> false

(** [box_apply f a] maps the function [f] into the binding box [a].  Note that
    this is equivalent to [apply_box (box f) a], but it is more efficient. *)
let box_apply : ('a -> 'b) -> 'a box -> 'b box = fun f a ->
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
type data = bool * any_var list * int

(** [gather_data acc b] collects some data about a [box] [b]. The informations
    obtained contain a boolean indicating whether the binding box is formed of
    a [Box] constructor, the list of all the variables and the number of slots
    reserved in the environment. This data is accumulated into [acc]. *)
let gather_data (only_box, vs_acc, n_acc) b =
  match b with
  | Box(_)      -> (only_box, vs_acc              , n_acc      )
  | Env(vs,n,_) -> (false   , merge_uniq vs_acc vs, max n_acc n)

(** [no_data] is in some sense the neutral element of [gather_data]. *)
let no_data : data = (true, [], 0)

(** [box_to_closure b] extracts a ['a closure] from a ['a box]. In the process
    the variables and the reserved slots are lost.  This function is  intended
    to be used in conjunction with [gather_data]. *)
let box_to_closure : 'a box -> 'a closure = fun b ->
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
    permute the [box] type with another type constructor). *)
module Lift(M : Map) =
  struct
    let lift_box_aux m = fun env -> M.map (fun o -> o env) m
    let lift_box : 'a box M.t -> 'a M.t box =
      fun m ->
        let data = ref no_data in
        let fn b = data := gather_data !data b; box_to_closure b in
        let m = M.map fn m in
        let aux vp =
          let m = M.map (fun o -> o vp) m in
          lift_box_aux m
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
    let lift_box_aux m = fun env -> M.map (fun o -> o env) (fun o -> o env) m
    let lift_box : ('a box, 'b box) M.t -> ('a,'b) M.t box =
      fun m ->
        let data = ref no_data in
        let fn b = data := gather_data !data b; box_to_closure b in
        let m = M.map fn fn m in
        let aux vp =
          let m = M.map (fun o -> o vp) (fun o -> o vp) m in
          lift_box_aux m
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

(** [unbox b] takes out the element that is in the ['a box] b.  In particular,
    if some variable have not been bound, they their [mkfree] filed is used to
    make them into elements of the expected type.  The [unbox] function should
    not be called until the construction of a term is finished (i.e., until no
    more variables need to be bound). *)
let unbox : 'a box -> 'a = fun b ->
  match b with
  | Box(t)       -> t
  | Env(vs,nb,t) ->
      let nbvs = List.length vs in
      let env = Env.create ~next_free:nbvs (nbvs + nb) in
      let cur = ref 0 in
      let fn vp (V(x)) =
        let i = !cur in incr cur;
        Env.set env i (x.var_mkfree x);
        IMap.add x.var_key i vp
      in
      t (List.fold_left fn IMap.empty vs) env

(** The representation of a [binder],  which is an element of type ['b] with a
    bound variable of type ['a]. *)
type ('a,'b) binder =
  { b_name   : string       (** Name of the bound variable.            *)
  ; b_bind   : bool         (** Indicates whether the variable occurs. *)
  ; b_rank   : int          (** Number of remaining free variables.    *)
  ; b_mkfree : 'a var -> 'a (** Injection of variables into domain.    *)
  ; b_value  : 'a -> 'b     (** Substitution function.                 *) }

(** [binder_name] returns the name of the variable bound by the [binder]. *)
let binder_name : ('a,'b) binder -> string = fun b -> b.b_name

(** [subst b v] substitutes the variable bound by [b], using [v]. *)
let subst : ('a,'b) binder -> 'a -> 'b = fun b x -> b.b_value x

(** [binder_occur b] tests whether the bound variable occurs in [b]. *)
let binder_occur : ('a,'b) binder -> bool = fun b -> b.b_bind

(** [binder_constant b] tests whether the [binder] [b] is constant (i.e.,  its
    bound variable does not occur). *)
let binder_constant : ('a,'b) binder -> bool = fun b -> not b.b_bind

(** [binder_closed b] test whether the [binder] [b] is closed (i.e.,  does not
    contain any free variable). *)
let binder_closed : ('a,'b) binder -> bool = fun b -> b.b_rank = 0

(** [binder_rank b] gives the number of free variables contained in [b]. *)
let binder_rank : ('a,'b) binder -> int = fun b -> b.b_rank

(** The representation of a multiple binder,  which binds several variables at
    once. It corresponds to an expression of type ['b] with bound variables of
    type ['a]. *)
type ('a,'b) mbinder =
  { mb_names  : string array   (** Names of the bound variables.          *)
  ; mb_binds  : bool array     (** Indicates whether the variables occur. *)
  ; mb_rank   : int            (** Number of remaining free variables.    *)
  ; mb_mkfree : 'a var -> 'a   (** Injection of variables into domain.    *)
  ; mb_value  : 'a array -> 'b (** Substitution function.                 *) }

(** [mbinder_arity b] gives the arity of the [mbinder]. *)
let mbinder_arity : ('a,'b) mbinder -> int = fun b -> Array.length b.mb_names

(** [mbinder_names b] return the array of the names of the variables bound  by
    the [mbinder] [b]. *)
let mbinder_names : ('a,'b) mbinder -> string array = fun b -> b.mb_names

(** [msubst b vs] substitutes the variables bound by [b], using the array [vs]
    (whose size should be equal to [mbinder_arity b]). *)
let msubst : ('a,'b) mbinder -> 'a array -> 'b = fun b xs -> b.mb_value xs

(** [mbinder_occurs b] returns an array of [bool] indicating if the  variables
    that are bound occur (i.e., are used). *)
let mbinder_occurs : ('a,'b) mbinder -> bool array = fun b -> b.mb_binds

(** [mbinder_constant b] indicates whether the [mbinder] [b] is constant. This
    means that none of its variables are used. *)
let mbinder_constant : ('a,'b) mbinder -> bool =
  fun b -> not (Array.fold_left (||) false b.mb_binds)

(** [mbinder_closed b] indicates whether [b] is closed. *)
let mbinder_closed : ('a,'b) mbinder -> bool = fun b -> b.mb_rank = 0

(** [mbinder_rank b] gives the number of free variables contained in [b]. *)
let mbinder_rank : ('a,'b) mbinder -> int = fun b -> b.mb_rank

(** [dummy_box] can be used for initialising structures like arrays. Note that
    if [unbox] is called on a data structure containing [dummy_box],  then the
    exception [Failure "Invalid use of dummy_box"] is raised. *)
let dummy_box : 'a box =
  let fail _ = failwith "Invalid use of dummy_box" in
  Env([], 0, fail)

(** [build_var key mkfree name] initialises a new ['a var] structure using the
    given data, and updates the [var_box] field accordingly. *)
let build_var_aux key vp = Env.get (IMap.find key vp)
let build_var : int ->  ('a var -> 'a) -> string -> 'a var =
  fun var_key var_mkfree name ->
    let rec x =
      let var_box = Env([V(x)], 0, build_var_aux var_key) in
      {var_key; var_name = name; var_mkfree; var_box}
    in x

(** [new_var mkfree name] create a new free variable using a wrapping function
    [mkfree] and a default [name]. *)
let new_var : ('a var -> 'a) -> string -> 'a var =
  fun mkfree s -> build_var (fresh_key ()) mkfree s

(** [new_mvar mkfree names] creates an array of new free variables in the same
    way as [new_var] does. *)
let new_mvar : ('a var -> 'a) -> string array -> 'a mvar =
  fun mkfree names -> Array.map (fun n -> new_var mkfree n) names

(** [copy_var x name mkfree] makes a copy of variable [x],  with a potentially
    different name and syntactic wrapper. However, the copy is treated exactly
    as the original in terms of binding and substitution. *)
let copy_var : 'b var -> ('a var -> 'a) -> string -> 'a var =
  fun x mkfree -> build_var x.var_key mkfree

(** [build_binder x rank bind value] constructs a binder with the given values
    (the variable [x] is used to obtain the name of the bound variable). *)
let build_binder : 'a var -> int -> bool -> ('a -> 'b) -> ('a,'b) binder =
  fun x b_rank b_bind b_value ->
    let b_name = x.var_name in
    {b_name; b_rank; b_bind; b_value; b_mkfree = x.var_mkfree}

(** [bind_var x b] produces a [binder] (in a [box]) by binding [x] in [b].
    This is one of the main [Bindlib] functions. *)
let bind_var_aux1 n t = fun arg ->
  let env = Env.create ~next_free:1 (n+1) in
  Env.set env 0 arg; t env
let bind_var_aux2 rank t = fun env arg ->
  let next = Env.get_next_free env in
  if next = rank then
    begin
      Env.set_next_free env (next + 1);
      Env.set env next arg; t env
    end
  else
    begin
      let env = Env.copy env in
      Env.set_next_free env (rank+1);
      for i = rank+1 to next-1 do Env.set env i 0 done;
      Env.set env rank arg; t env
    end
let bind_var_aux3 x rank t = fun env ->
  let value = bind_var_aux2 rank t env in
  build_binder x rank true value
let bind_var_aux4 t = fun env _ -> t env
let bind_var_aux5 x rank t = fun env ->
  let value = bind_var_aux4 t env in
  build_binder x rank false value

let bind_var : 'a var -> 'b box -> ('a, 'b) binder box = fun x b ->
  match b with
  | Box(t)      -> Box(build_binder x 0 false (fun _ -> t))
  | Env(vs,n,t) ->
      try
        match vs with
        | [V(y)] ->
            if x.var_key <> y.var_key then raise Not_found;
            (* The variable to bind is the last one. *)
            let r = 0 in
            let t = t (IMap.singleton x.var_key r) in
            let value = bind_var_aux1 n t in
            Box(build_binder x 0 true value)
        | _      ->
            let vs = remove x vs in
            (* General case. *)
            let cl vp =
              let rank = List.length vs in
              let r = rank in
              let t = t (IMap.add x.var_key r vp) in
              bind_var_aux3 x rank t
            in
            Env(vs, n+1, cl)
      with Not_found ->
        (* The variable does not occur. *)
        let value vp =
          let t = t vp in
          let rank = List.length vs in
          bind_var_aux5 x rank t
        in Env(vs, n, value)

(** [check_arity xs args] matches the size of [xs] and [args],  and raises the
    [Invalid_argument "Bad arity in msubst"] exception in case of failure. The
    error can only be triggered at runtime (in [msubst]). *)
let check_arity : 'a mvar -> 'a array -> unit = fun xs args ->
  if Array.(length xs <> length args) then invalid_arg "Bad arity in msubst"

(** [bind_mvar xs b] produces a [mbinder] (in a [box]) by binding [xs]  in
    [b], in a similar way as [bind_var] does for single variables. *)
let bind_mvar_aux0 xs t = fun args -> check_arity xs args; t
let bind_mvar_aux1 m xs mb_binds t = fun args ->
  check_arity xs args;
  let v = Env.create m in
  let pos = ref 0 in
  for i = 0 to Array.length xs - 1 do
    if mb_binds.(i) then begin
        Env.set v !pos args.(i);
        incr pos;
      end
  done;
  Env.set_next_free v !pos;
  t v
let bind_mvar_aux2 xs t = fun env args -> check_arity xs args; t env
let bind_mvar_aux3 xs t mb_names mb_rank mb_binds mb_mkfree = fun env ->
  let mb_value = bind_mvar_aux2 xs t env in
  {mb_names; mb_rank; mb_binds; mb_value; mb_mkfree}
let bind_mvar_aux4 xs t mb_rank mb_binds = fun env args ->
  check_arity xs args;
  let next = Env.get_next_free env in
  let cur_pos = ref mb_rank in
  if next = mb_rank then
    begin
      for i = 0 to Array.length xs - 1 do
        if mb_binds.(i) then begin
            Env.set env !cur_pos args.(i);
            incr cur_pos;
          end
      done;
      Env.set_next_free env !cur_pos;
      t env
    end
  else
    begin
      let env = Env.copy env in
      for i = 0 to Array.length xs - 1 do
        if mb_binds.(i) then begin
            Env.set env !cur_pos args.(i);
            incr cur_pos;
          end
      done;
      Env.set_next_free env !cur_pos;
      for i = !cur_pos to next - 1 do Env.set env i 0 done;
      t env
    end
let bind_mvar_aux5 xs t mb_names mb_rank mb_binds mb_mkfree = fun env ->
  let mb_value = bind_mvar_aux4 xs t mb_rank mb_binds env in
  {mb_names; mb_rank; mb_binds; mb_value; mb_mkfree}

let bind_mvar : 'a mvar -> 'b box -> ('a,'b) mbinder box = fun xs b ->
  let mb_mkfree =
    if Array.length xs > 0 then xs.(0).var_mkfree
    else (fun _ -> assert false)
  in
  match b with
  | Box(t)      ->
      let mb_binds = Array.map (fun _ -> false) xs in
      let mb_names = Array.map name_of xs in
      let mb_value = bind_mvar_aux0 xs t in
      Box({mb_names; mb_rank = 0; mb_binds; mb_value; mb_mkfree})
  | Env(vs,n,t) ->
      let keys = Array.map (fun _ -> 0) xs in
      let vss = Array.map (fun _ -> vs) xs in
      let (vs, m) =
        let vs = ref vs in let m = ref n in
        for i = Array.length xs - 1 downto 0 do
          let v = xs.(i) in
          begin
            try vs := remove v !vs; incr m; keys.(i) <- v.var_key
            with Not_found -> keys.(i) <- -1
          end;
          vss.(i) <- !vs (*NOTE: store each vs, for good renaming *)
        done; (!vs, !m)
      in
      if vs = [] then (* All the free variables become bound. *)
        let mb_names = Array.map (fun _ -> "") xs in
        let cur_pos = ref 0 in
        let vp = ref IMap.empty in
        let f i key =
          mb_names.(i) <- xs.(i).var_name;
          if key >= 0 then
            begin
              vp := IMap.add key !cur_pos !vp;
              incr cur_pos; true
            end
          else false
        in
        let mb_binds = Array.mapi f keys in
        let t = t !vp in
        let mb_value = bind_mvar_aux1 m xs mb_binds t in
        Box({mb_names; mb_binds; mb_rank = 0; mb_value; mb_mkfree})
      else if m = n then (* None of the variables occur. *)
        let cl vp =
          let mb_rank = List.length vs in
          let mb_binds = Array.map (fun _ -> false) xs in
          let fn x = x.var_name in
          let mb_names = Array.map fn xs in
          let t = t vp in
          bind_mvar_aux3 xs t mb_names mb_rank mb_binds mb_mkfree
        in Env(vs, n, cl)
      else (* General case. *)
        let cl vp =
          let mb_names = Array.map (fun _ -> "") xs in
          let mb_rank = List.length vs in
          let cur_pos = ref mb_rank in
          let vp = ref vp in
          let f i key =
            mb_names.(i) <- xs.(i).var_name;
            if key >= 0 then
              (vp := IMap.add key !cur_pos !vp;
               incr cur_pos; true)
            else false
          in
          let mb_binds = Array.mapi f keys in
          let t = t !vp in
          bind_mvar_aux5 xs t mb_names mb_rank mb_binds mb_mkfree
       in Env(vs, m, cl)

(** [unbind b] breaks the [binder] [b] into a variable and a body. The name of
    this variable is based on that of the binder. *)
let unbind : ('a,'b) binder -> 'a var * 'b = fun b ->
    let x = new_var b.b_mkfree (binder_name b) in
    (x, subst b (b.b_mkfree x))

(** [unbind2 f g] is similar to [unbind f], but it substitutes two binders [f]
    and [g] at once, using the same fresh variable. *)
let unbind2 : ('a,'b) binder -> ('a,'c) binder -> 'a var * 'b * 'c =
  fun b1 b2 ->
    let x = new_var b1.b_mkfree (binder_name b1) in
    let v = b1.b_mkfree x in
    (x, subst b1 v, subst b2 v)

(** Short name for the type of an equality function. *)
type 'a eq = 'a -> 'a -> bool

(** [eq_binder eq f g] tests the equality between the binders [f] and [g]. The
    binders are first substituted using the same fresh variable, and then [eq]
    is called on the resulting terms. *)
let eq_binder : 'b eq -> ('a,'b) binder eq = fun eq f g ->
  f == g || let (_,t,u) = unbind2 f g in eq t u

(** [unmbind mkfree b] breaks the [mbinder] [b] into an array of variables and
    a body. It is required to provide a [mkfree] function since [unmbind]  has
    to create new variables. Their names are besed on the bound variables. *)
let unmbind : ('a,'b) mbinder -> 'a mvar * 'b = fun b ->
  let x = new_mvar b.mb_mkfree (mbinder_names b) in
  (x, msubst b (Array.map b.mb_mkfree x))

(** [unmbind2 mkfree f g] is similar to [unmbind mkfree f], but it substitutes
    both [f] and [g] using the same fresh variables. Note that the two binders
    must have the same arity. *)
let unmbind2 : ('a,'b) mbinder -> ('a,'c) mbinder -> 'a mvar * 'b * 'c =
  fun b1 b2 ->
    if mbinder_arity b1 <> mbinder_arity b2 then
      invalid_arg "Arity missmatch in unmbind2";
    let xs = new_mvar b1.mb_mkfree (mbinder_names b1) in
    let vs = Array.map b1.mb_mkfree xs in
    (xs, msubst b1 vs, msubst b2 vs)

(** [eq_mbinder eq f g] tests the equality between two [mbinder] [f] and  [g].
    They are first substituted with the same fresh variables, and then [eq] is
    called on the resulting terms. *)
let eq_mbinder : 'b eq -> ('a,'b) mbinder eq = fun eq f g ->
  f == g ||
    (mbinder_arity f = mbinder_arity g &&
       let (_,t,u) = unmbind2 f g in eq t u)

(** [box_binder f b] boxes the binder [b] using the boxing function [f].  Note
    that when [b] is closed, it is immediately boxed using the [box] function.
    In that case, the function [f] is not used at all. *)
let box_binder : ('b -> 'b box) -> ('a,'b) binder -> ('a,'b) binder box =
  fun f b ->
    if b.b_rank = 0 then box b else
    let (x,t) = unbind b in
    bind_var x (f t)

(** [box_mbinder f b] boxes the multiple binder [b] using the boxings function
    [f]. Note that if [b] is closed then it is immediately boxed (with [box]),
    without relying on [f] at all. *)
let box_mbinder : ('b -> 'b box) -> ('a,'b) mbinder -> ('a,'b) mbinder box =
  fun f b ->
    if b.mb_rank = 0 then box b else
    let (xs,t) = unmbind b in
    bind_mvar xs (f t)

(** [bind_apply b arg] substitute a [binder] in the [box] type. *)
let bind_apply : ('a,'b) binder box -> 'a box -> 'b box =
  fun b arg -> box_apply2 subst b arg

(** [mbind_apply b args] substitute a [mbinder] in the [box] type. *)
let mbind_apply : ('a,'b) mbinder box -> 'a array box -> 'b box =
  fun b args -> box_apply2 msubst b args

module type Renaming = sig
  (** A type to represent set of variables *)
  type ctxt

  (** [empty_ctxt] is the empty context. *)
  val empty_ctxt : ctxt

  (** [new_name n s] From the original name [n] and a set [s], create a new free
     variable [n'] (not occuring in [s]) and returns [(n',s')] where [s'] is
     [s] with [n'] added *)
  val new_name : string -> ctxt -> string * ctxt
end

module Ctxt(R:Renaming) = struct
  include R

  (** [new_var_in ctxt mkfree name] is similar to [new_var mkfree name], but the
    variable names is chosen not to collide with the context [ctxt]. Note that
    the context that is returned contains the new variable name. *)
  let new_var_in : ctxt -> ('a var -> 'a) -> string -> 'a var * ctxt =
    fun ctxt mkfree name ->
      let (fresh_name, ctxt) = new_name name ctxt in
      let x = new_var mkfree fresh_name in
      (x, ctxt)

  (** [new_mvar_in ctxt mkfree names] is similar to [new_mvar mkfree names], but
      it handles the context (see [new_var_in]). *)
  let new_mvar_in : ctxt -> ('a var -> 'a) -> string array -> 'a mvar * ctxt =
    fun ctxt mkfree names ->
    let ctxt = ref ctxt in
    let f name =
      let (v, new_ctxt) = new_var_in !ctxt mkfree name in
      ctxt := new_ctxt; v
    in
    (Array.map f names, !ctxt)

  (** [unbind_in ctxt b] is similar to [unbind b], but it handles the context as
    explained in the documentation of [new_mvar_in]. *)
  let unbind_in : ctxt -> ('a,'b) binder -> 'a var * 'b * ctxt = fun ctxt b ->
    let ctxt = if binder_closed b then empty_ctxt else ctxt in
    let (x, ctxt) = new_var_in ctxt b.b_mkfree (binder_name b) in
    (x, subst b (b.b_mkfree x), ctxt)

  (** [munbind_in ctxt mkfree b] is like [munbind mkfree b],  but it handles the
      context (see [new_mvar_in]). *)
  let unmbind_in : ctxt -> ('a,'b) mbinder -> 'a mvar * 'b * ctxt =
    fun ctxt b ->
    let ctxt = if mbinder_closed b then empty_ctxt else ctxt in
    let (x, ctxt) = new_mvar_in ctxt b.mb_mkfree (mbinder_names b) in
    (x, msubst b (Array.map b.mb_mkfree x), ctxt)
end

module Default_Renaming = struct
  (** Representation of a context, or a list of reserved names. *)
  type ctxt = int list SMap.t

  (** [empty_ctxt] is the empty context. *)
  let empty_ctxt = SMap.empty

  (** [split_name s] splits [s] into a [string] prefix and an [int] suffix. Note
    that we have [split "xyz" = ("xyz", (-1))], [split "xyz12" = ("xyz", 12)],
    or [split "12" = ("", 12)]. In other words, we take the longest suffix. In
    particular, [split "xyz007"] and [split "xyz7"] both yield the same value,
    which is [("xyz", 7)]. *)
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

  (** [merge_name prefix suffix] builds a variable name using a [string]  prefix
      and an [int] suffix. *)
  let merge_name : string -> int -> string =
    fun pr sf -> if sf >= 0 then pr ^ (string_of_int sf) else pr

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

  let new_name : string -> ctxt -> string * ctxt =
    fun name ctxt ->
      let prefix, suffix = split_name name in
      let suffix, ctxt = get_suffix prefix suffix ctxt in
      (merge_name prefix suffix, ctxt)
end

module Default_Ctxt = Ctxt(Default_Renaming)

include Default_Ctxt

(** [binder_compose b f] postcomposes the binder [b] with the function [f]. In
    the process, the binding structure is not changed. Note that this function
    is not always safe. Use it with care. *)
let binder_compose : ('a,'b) binder -> ('b -> 'c) -> ('a,'c) binder =
  fun b f -> {b with b_value = (fun x -> f (b.b_value x))}

(** [mbinder_compose b f] postcomposes the multiple binder [b] with [f].  This
    function is similar to [binder_compose], and it is not always safe. *)
let mbinder_compose : ('a,'b) mbinder -> ('b -> 'c) -> ('a,'c) mbinder =
  fun b f -> {b with mb_value = (fun x -> f (b.mb_value x))}

(** [raw_binder name bind rank mkfree value] builds a binder using the [value]
    function as its definition. The parameter [name] correspond to a preferred
    name of the bound variable, the boolean [bind] indicates whether the bound
    variable occurs, and [rank] gives the number of distinct free variables in
    the produced binder. The [mkfree] function injecting variables in the type
    ['a] of the domain of the binder must also be given. This function must be
    considered unsafe because it is the responsibility of the user to give the
    accurate value for [bind] and [rank]. *)
let raw_binder : string -> bool -> int -> ('a var -> 'a) -> ('a -> 'b)
    -> ('a,'b) binder =
  fun b_name b_bind b_rank b_mkfree b_value ->
    {b_name; b_bind; b_rank; b_mkfree; b_value}

(** [raw_mbinder names binds rank mk_free value] is similar  to  [raw_binder],
    but it is applied to a multiple binder. As for [raw_binder], this function
    has to be considered unsafe because the user must enforce invariants. *)
let raw_mbinder : string array -> bool array -> int -> ('a var -> 'a)
    -> ('a array -> 'b) -> ('a,'b) mbinder =
  fun mb_names mb_binds mb_rank mb_mkfree mb_value ->
    {mb_names; mb_binds; mb_rank; mb_mkfree; mb_value}
