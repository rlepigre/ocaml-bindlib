(** The [Bindlib] library provides support for free and bound variables in the
    OCaml language. The main application is the representation of types with a
    binding structure (e.g., abstract syntax trees).

    @author Christophe Raffalli
    @author Rodolphe Lepigre
    @version 6.0.0 *)

(* Counter for generating fresh variable keys (i.e., unique identifiers). *)
let ((reset_counter : unit -> unit), (fresh_key : unit -> int)) =
  let c = ref (-1) in
  ((fun () -> c := -1), (fun () -> incr c; !c))

(* FIXME remove in recent enough OCaml. *)
module Int = struct
  type t = int
  let compare = (-)
end

module IMap = Map.Make(Int)
module SMap = Map.Make(String)

(* Environment **************************************************************)

(* The [Bindlib] library uses environments to store values associated to bound
   variables upon substitution. We rely on the [Obj] module to store variables
   with potentially different types in a single array.

   Uses of the [Env] module by [Bindlib] are safe because every single cell of
   every environment is only used at a single, fixed type. Moreover, there can
   be no problem with unboxed floating-point arrays because it is not possible
   to extend the [float] type itself with variables. *)

module Env : sig
  (* Type of an environment. *)
  type t

  (* [create next_free size] creates a new empty environment of with the given
     [size], and with its next free cell at index [next_free]. *)
  val create : int -> int -> t

  (* [set env i e] sets the value stored at position [i] of [env] to [e]. *)
  val set : t -> int -> 'a -> unit

  (* [get i env] gets the value stored at position [i] in [env]. *)
  val get : int -> t -> 'a

  (* [blit s t n] copies the [n] first elements of [s] to [t]. *)
  val blit : t -> t -> int -> unit

  (* [copy env] make a copy of environment [env]. *)
  val copy : t -> t

  (* [get_next_free env] returns the index of the next free cell in [env]. *)
  val get_next_free : t -> int

  (* [set_next_free env i] sets the next free cell index of [env] to [i]. *)
  val set_next_free : t -> int -> unit
end = struct
  type t = {tab : Obj.t array; mutable next_free : int}
  let create next_free size = {tab = Array.make size (Obj.repr ()); next_free}
  let set env i e = Array.set env.tab i (Obj.repr e)
  let get i env = Obj.obj (Array.get env.tab i)
  let blit src dst len = Array.blit src.tab 0 dst.tab 0 len
  let copy env = {tab = Array.copy env.tab; next_free = env.next_free}
  let get_next_free env = env.next_free
  let set_next_free env n = env.next_free <- n
end

(* Closure representation ***************************************************)

(* In the internals of [Bindlib], variables are identified by a unique integer
   key. Closures are then formed by mapping each free variable to a slot in an
   environment (of type [Env.t]), but not directly. The mapping is established
   using a map (of type [varpos]) which associates variable keys to indices in
   the environment. *)

type varpos = int IMap.t

type 'a closure = varpos -> Env.t -> 'a

let map_closure_aux f a = fun env -> f (a env)
let map_closure : ('a -> 'b) -> 'a closure -> 'b closure =
  fun f cla vs -> map_closure_aux f (cla vs)

let app_closure_aux f a = fun env -> f env a
let app_closure : ('a -> 'b) closure -> 'a -> 'b closure =
  fun clf a vs -> app_closure_aux (clf vs) a

let apply_closure_aux f a = fun env -> f env (a env)
let (<*>) : ('a -> 'b) closure -> 'a closure -> 'b closure =
  fun clf cla vs -> apply_closure_aux (clf vs) (cla vs)

(* Box and variable representation ******************************************)

(* At the core of [Bindlib] is the type ['a box] which represents an object of
   type ['a] whose free variables are available for binding. Intuitively, this
   is represented as the combination of an environment and a closure as in the
   [Env(vs,n,t)]. The [Box(e)] constructor's only role is to optimise the case
   where an object is known to be closed.

   In the [Env(vs,n,t)] constructor, the list [vs] contains all free variables
   in scope (sorted by their keys), the integer [n] counts the number of bound
   variables that have a reserved slot in the environment, and finally, [t] is
   a representation of the objects (with variables) using a closure.

   Note that the closure [t] is intended to receive the environment as  second
   argument, and the position of the free variables of [vs] in the environment
   as a first argument.

   Important remark: to be able to efficiently implement substitution, we make
   sure that the [varpos] map (given as first argument to closures) is used at
   most once for each variable, even if a variable appears many times. *)

type 'a box =
  | Box of 'a                              (* Closed object.                *)
  | Env of any_var list * int * 'a closure (* Open object with environment. *)

and 'a var = {
  var_key    : int;          (* Unique identifier.          *)
  var_name   : string;       (* Name as a free variable.    *)
  var_mkfree : 'a var -> 'a; (* Function to build a term.   *)
  var_box    : 'a box;       (* Variable as a boxed object. *)
}

(* Variable of any type (using an existential). In principle, this constructor
   can be unboxed since it has a single constructor.  However, this only works
   starting from OCaml 4.11.0. The annotation is erased for prior versions. *)
and any_var = V : 'a var -> any_var
[@@unboxed]

let name_of x = x.var_name
let hash_var x = Hashtbl.hash (`HVar, x.var_key)
let box_var x = x.var_box
let compare_vars x y = y.var_key - x.var_key
let eq_vars x y = x.var_key = y.var_key
let uid_of x = x.var_key

type 'a mvar = 'a var array

let names_of xs = Array.map name_of xs
let uids_of xs = Array.map uid_of xs

(* The [merge_uniq] and [remove] functions manipulate variables lists found in
   boxed object of the form [Env(_,_,_)]. They rely on the fact that such list
   is always sorted in the order of variable keys, and has no duplicates. *)

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

let remove : 'a var -> any_var list -> any_var list = fun {var_key ; _} ->
  let rec remove acc = function
    | (V(x) as v)::l when x.var_key < var_key -> remove (v::acc) l
    | (V(x)     )::l when x.var_key = var_key -> List.rev_append acc l
    | _                                       -> raise Not_found
  in remove []

(* Function [minimize vs n cl] constructs a minimal closure equivalent to [cl]
   and only containing variables of [vs]. Additionally, the function builds an
   environment with [n] extra slots. *)

let minimize_aux_prefix size n t = fun env ->
  let new_env = Env.create size (size + n) in
  Env.blit env new_env size; t new_env
let minimize_aux tab n t = fun env ->
  let size = Array.length tab in
  let new_env = Env.create size (size + n) in
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
      if i <> j then prefix := false;
      tab.(i) <- j;
      (IMap.add var.var_key i new_vp, i+1)
    in
    let (new_vp,_) = List.fold_left f (IMap.empty,0) vs in
    let t = t new_vp in
    if !prefix then minimize_aux_prefix size n t else minimize_aux tab n t

let box : 'a -> 'a box = fun t -> Box (t)

let apply_box : ('a -> 'b) box -> 'a box -> 'b box = fun f a ->
  match (f, a) with
  | (Box(f)       , Box(a)       ) -> Box(f a)
  | (Box(f)       , Env(va,na,ta)) -> Env(va, na, map_closure f ta)
  | (Env(vf,nf,tf), Box(a)       ) -> Env(vf, nf, app_closure tf a)
  | (Env(vf,nf,tf), Env(va,na,ta)) ->
      Env(merge_uniq vf va, 0, minimize vf nf tf <*> minimize va na ta)

let occur : 'a var -> 'b box -> bool = fun v b ->
  match b with
  | Box(_)      -> false
  | Env(vs,_,_) -> List.exists (fun (V(x)) -> x.var_key = v.var_key) vs

let is_closed : 'a box -> bool = fun b ->
  match b with Box(_) -> true | Env(_,_,_) -> false

let box_apply : ('a -> 'b) -> 'a box -> 'b box = fun f a ->
  match a with
  | Box(a)        -> Box(f a)
  | Env(vs,na,ta) -> Env(vs, na, map_closure f ta)

let box_apply2 f ta tb       = apply_box (box_apply f ta) tb
let box_apply3 f ta tb tc    = apply_box (box_apply2 f ta tb) tc
let box_apply4 f ta tb tc td = apply_box (box_apply3 f ta tb tc) td

let box_pair x y = box_apply2 (fun x y -> (x,y)) x y
let box_triple x y z = box_apply3 (fun x y z -> (x,y,z)) x y z

let box_opt o =
  match o with
  | None    -> box None
  | Some(e) -> box_apply (fun e -> Some(e)) e

let dummy_box : 'a box =
  let fail _ = failwith "Invalid use of dummy_box" in
  Env([], 0, fail)

let unbox b =
  match b with Box(t) -> t | Env(vs,nb,t) ->
  let nbvs = List.length vs in
  let env = Env.create nbvs (nbvs + nb) in
  let cur = ref 0 in
  let fn vp (V(x)) =
    let i = !cur in incr cur;
    Env.set env i (x.var_mkfree x);
    IMap.add x.var_key i vp
  in
  t (List.fold_left fn IMap.empty vs) env

(* Functorial interface for boxing functions ********************************)

(* We use the following type and functions for combining several boxed objects
   into one. In particular, we combine all lists of variables [vs] that appear
   in the encountered [Env(vs,n,_)] constructors. We also remember the largest
   [n] encountered in such constructors, and whether we encountered only [Box]
   constructors (i.e., closed objects).

   The type [data] is used to represent these three pieces of information. The
   function [gather_data data b] combines data gathered so far (in [data]) and
   new data from a boxed object [b]. Note that [no_data] plays the role of the
   neutral element of [gather_data]. *)

type data = bool * any_var list * int

let gather_data : data -> 'a box -> data = fun data b ->
  match (b, data) with
  | (Box(_)     , _       ) -> data
  | (Env(vs,n,_), (_,ws,m)) -> (false, merge_uniq ws vs, max m n)

let no_data : data = (true, [], 0)

(* Function [box_to_closure b] constructs a closure from boxed object [b]. The
   information about variables and reserved slots (contained in boxed objects)
   is lost in the process, and it must thus be tracked independently (with the
   use of [gather_data]) to reconstruct a boxed object in the end. *)

let box_to_closure : 'a box -> 'a closure = fun b ->
  match b with
  | Box(t)      -> fun _ _ -> t
  | Env(vs,n,t) -> minimize vs n t

module type Map = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Lift(M : Map) = struct
  let lift_box_aux m = fun env -> M.map (fun o -> o env) m
  let lift_box : 'a box M.t -> 'a M.t box = fun m ->
    let data = ref no_data in
    let fn b = data := gather_data !data b; box_to_closure b in
    let m = M.map fn m in
    let aux vp =
      let m = M.map (fun o -> o vp) m in
      lift_box_aux m
    in
    match !data with
    | (true, _ , _) -> Box(aux IMap.empty (Env.create 0 0))
    | (_   , vs, n) -> Env(vs, n, minimize vs n aux)
end

module type Map2 = sig
  type ('a, 'b) t
  val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
end

module Lift2(M : Map2) = struct
  let lift_box_aux m = fun env -> M.map (fun o -> o env) (fun o -> o env) m
  let lift_box : ('a box, 'b box) M.t -> ('a,'b) M.t box = fun m ->
    let data = ref no_data in
    let fn b = data := gather_data !data b; box_to_closure b in
    let m = M.map fn fn m in
    let aux vp =
      let m = M.map (fun o -> o vp) (fun o -> o vp) m in
      lift_box_aux m
    in
    match !data with
    | (true, _ , _) -> Box(aux IMap.empty (Env.create 0 0))
    | (_   , vs, n) -> Env(vs, n, minimize vs n aux)
end

module Lift_list = Lift(
  struct
    type 'a t = 'a list
    let map = List.map
  end)
let box_list = Lift_list.lift_box

module Lift_rev_list = Lift(
  struct
    type 'a t = 'a list
    let map = List.rev_map
  end)
let box_rev_list = Lift_rev_list.lift_box

module Lift_array = Lift(
  struct
    type 'a t = 'a array
    let map = Array.map
  end)
let box_array = Lift_array.lift_box

(* Representation of binders ************************************************)

type ('a,'b) binder = {
  b_name   : string;       (* Preferred name for the bound variable. *)
  b_bind   : bool;         (* Indicates whether the variable occurs. *)
  b_rank   : int;          (* Number of remaining free variables.    *)
  b_mkfree : 'a var -> 'a; (* Injection of variables into domain.    *)
  b_value  : 'a -> 'b;     (* Substitution function.                 *)
}

let binder_name b = b.b_name
let subst b x = b.b_value x
let binder_occur b = b.b_bind
let binder_constant b = not b.b_bind
let binder_closed b = b.b_rank = 0
let binder_rank b = b.b_rank
let binder_compose b f = {b with b_value = (fun x -> f (b.b_value x))}

type ('a,'b) mbinder = {
  mb_names  : string array;   (* Preferred names for the bound variables. *)
  mb_binds  : bool array;     (* Indicates whether the variables occur.   *)
  mb_rank   : int;            (* Number of remaining free variables.      *)
  mb_mkfree : 'a var -> 'a;   (* Injection of variables into domain.      *)
  mb_value  : 'a array -> 'b; (* Substitution function.                   *)
}

let mbinder_arity b = Array.length b.mb_names
let mbinder_names b = b.mb_names
let msubst b xs = b.mb_value xs
let mbinder_occurs b = b.mb_binds
let mbinder_constant b = not (Array.fold_left (||) false b.mb_binds)
let mbinder_closed b = b.mb_rank = 0
let mbinder_rank b = b.mb_rank
let mbinder_compose b f = {b with mb_value = (fun x -> f (b.mb_value x))}

let raw_binder b_name b_bind b_rank b_mkfree b_value =
  {b_name; b_bind; b_rank; b_mkfree; b_value}

let raw_mbinder mb_names mb_binds mb_rank mb_mkfree mb_value =
  {mb_names; mb_binds; mb_rank; mb_mkfree; mb_value}

let bind_apply b arg = box_apply2 subst b arg
let mbind_apply b args = box_apply2 msubst b args

(* Variable creation ********************************************************)

let build_var_aux key vp = Env.get (IMap.find key vp)
let build_var var_key var_mkfree name =
  let rec x =
    let var_box = Env([V(x)], 0, build_var_aux var_key) in
    {var_key; var_name = name; var_mkfree; var_box}
  in x

let new_var mkfree name = build_var (fresh_key ()) mkfree name
let new_mvar mkfree names = Array.map (fun n -> new_var mkfree n) names

let copy_var x mkfree = build_var x.var_key mkfree

let unbind b =
  let x = new_var b.b_mkfree (binder_name b) in
  (x, subst b (b.b_mkfree x))

let unbind2 b1 b2 =
  let x = new_var b1.b_mkfree (binder_name b1) in
  let v = b1.b_mkfree x in
  (x, subst b1 v, subst b2 v)

let eq_binder eq f g = f == g || let (_,t,u) = unbind2 f g in eq t u

let unmbind b =
  let x = new_mvar b.mb_mkfree (mbinder_names b) in
  (x, msubst b (Array.map b.mb_mkfree x))

let unmbind2 b1 b2 =
  if mbinder_arity b1 <> mbinder_arity b2 then
    invalid_arg "Arity mismatch in unmbind2";
  let xs = new_mvar b1.mb_mkfree (mbinder_names b1) in
  let vs = Array.map b1.mb_mkfree xs in
  (xs, msubst b1 vs, msubst b2 vs)

let eq_mbinder eq f g =
  f == g ||
  (mbinder_arity f = mbinder_arity g && let (_,t,u) = unmbind2 f g in eq t u)

(* Implementation of [bind_var] *********************************************)

let build_binder x b_rank b_bind b_value =
  {b_name = x.var_name; b_rank; b_bind; b_value; b_mkfree = x.var_mkfree}

let bind_var_aux1 n t = fun arg ->
  let env = Env.create 1 (n+1) in
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

let box_binder f b =
  if binder_closed b then box b else
  let (x,t) = unbind b in
  bind_var x (f t)

(* Implementation of [bind_mvar] ********************************************)

let check_arity : 'a mvar -> 'a array -> unit = fun xs args ->
  if Array.(length xs <> length args) then invalid_arg "Bad arity in msubst"

let bind_mvar_aux0 xs t = fun args -> check_arity xs args; t
let bind_mvar_aux1 m xs mb_binds t = fun args ->
  check_arity xs args;
  let v = Env.create 0 m in
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

let box_mbinder f b =
  if b.mb_rank = 0 then box b else
  let (xs,t) = unmbind b in
  bind_mvar xs (f t)

(* Functorial interface for renaming policies *******************************)

module type Renaming = sig
  type ctxt
  val empty_ctxt : ctxt

  val reset_context_for_closed_terms : bool
  val skip_constant_binders : bool
  val constant_binder_name : string option

  val new_name : string -> ctxt -> string * ctxt
  val reserve_name : string -> ctxt -> ctxt
end

module Ctxt(R:Renaming) = struct
  include R

  let free_vars b =
    match b with
    | Box(_)      -> empty_ctxt
    | Env(vs,_,_) ->
        let add ctxt (V(x)) = reserve_name (name_of x) ctxt in
        List.fold_left add empty_ctxt vs

  let new_var_in ctxt mkfree name =
    let (fresh_name, ctxt) = new_name name ctxt in
    (new_var mkfree fresh_name, ctxt)

  let new_mvar_in ctxt mkfree names =
    let ctxt = ref ctxt in
    let f name =
      let (v, new_ctxt) = new_var_in !ctxt mkfree name in
      ctxt := new_ctxt; v
    in
    (Array.map f names, !ctxt)

  let reset_ctxt closed ctxt =
    if reset_context_for_closed_terms && closed then empty_ctxt else ctxt

  let unbind_var constant mkfree name ctxt =
    match (constant, skip_constant_binders, constant_binder_name) with
    | (true, _   , Some(s)) -> (new_var mkfree s, ctxt)
    | (true, true, None   ) -> (fst (new_var_in ctxt mkfree name), ctxt)
    | _                     -> new_var_in ctxt mkfree name

  let unbind_in ctxt b =
    let ctxt = reset_ctxt (binder_closed b) ctxt in
    let (x, ctxt) =
      let constant = binder_constant b in
      unbind_var constant b.b_mkfree (binder_name b) ctxt
    in
    (x, subst b (b.b_mkfree x), ctxt)

  let unbind2_in ctxt b1 b2 =
    let ctxt = reset_ctxt (binder_closed b1 && binder_closed b2) ctxt in
    let (x, ctxt) =
      let constant = binder_constant b1 && binder_constant b2 in
      unbind_var constant b1.b_mkfree (binder_name b1) ctxt
    in
    let v = b1.b_mkfree x in
    (x, subst b1 v, subst b2 v, ctxt)

  let unmbind_in ctxt b =
    let ctxt = reset_ctxt (mbinder_closed b) ctxt in
    let ctxt_ref = ref ctxt in
    let xs =
      let fn i name =
        let constant = not b.mb_binds.(i) in
        let (x, ctxt) = unbind_var constant b.mb_mkfree name !ctxt_ref in
        ctxt_ref := ctxt; x
      in
      Array.mapi fn b.mb_names
    in
    (xs, msubst b (Array.map b.mb_mkfree xs), !ctxt_ref)

  let unmbind2_in ctxt b1 b2 =
    let ctxt = reset_ctxt (mbinder_closed b1 && mbinder_closed b2) ctxt in
    let ctxt_ref = ref ctxt in
    let xs =
      let fn i name =
        let constant = not b1.mb_binds.(i) && not b1.mb_binds.(i) in
        let (x, ctxt) = unbind_var constant b1.mb_mkfree name !ctxt_ref in
        ctxt_ref := ctxt; x
      in
      Array.mapi fn b1.mb_names
    in
    let vs = Array.map b1.mb_mkfree xs in
    (xs, msubst b1 vs, msubst b2 vs, !ctxt_ref)
end

(* Default renaming policy (functor instantiation) **************************)

include Ctxt(struct
  (* Our renaming policy views variable names as a pair of a “prefix” and of a
     decimal “suffix”. For example, ["xyz42"] corresponds to [("xyz", 42)]. If
     a name does not have a decimal suffix, then the suffix value [0] is used.
     This means that ["xyz"] corresponds to [("xyz", 0)]. Moreover, all zeroes
     leading the decimal suffix are considered to be part of the prefix. Thus,
     name ["xyz0042"] corresponds to [("xyz00", 42)], and ["xyz0"] corresponds
     to [("xyz0", 0)].

     Following the above convention, one way to represent contexts would be to
     have a map from prefixes to sets of suffixes. We could then check whether
     a name is in the context by checking whether its prefix is already mapped
     to a set containing its suffix. However, this is not what we do.

     Instead, we over-approximate the set of names contained in a context, and
     only store a map from prefixes to the largest used suffix. So, when there
     is a mapping from ["xyz"] to [42] in the context, this means that all the
     names of the form [("xyz", i)] are taken for [i <= 42]. *)

  type ctxt = int SMap.t
  let empty_ctxt = SMap.empty

  let reset_context_for_closed_terms = false
  let skip_constant_binders = false
  let constant_binder_name = None

  let split_name : string -> string * int = fun name ->
    let len = String.length name in
    (* [i] is the index of the first first character of the suffix. *)
    let i =
      let is_digit c = '0' <= c && c <= '9' in
      let first_digit = ref len in
      let first_non_0 = ref len in
      while !first_digit > 0 && is_digit name.[!first_digit - 1] do
        decr first_digit;
        if name.[!first_digit] <> '0' then first_non_0 := !first_digit
      done;
      !first_non_0
    in
    if i = len then
      (name, 0)
    else
      (String.sub name 0 i, int_of_string (String.sub name i (len - i)))

  let get_suffix : string -> int -> ctxt -> int*ctxt = fun name suffix ctxt ->
    let n = try SMap.find name ctxt with Not_found -> -1 in
    let suffix = if suffix > n then suffix else n + 1 in
    (suffix, SMap.add name suffix ctxt)

  let merge_name : string -> int -> string = fun prefix suffix ->
    if suffix > 0 then prefix ^ string_of_int suffix else prefix

  let new_name : string -> ctxt -> string * ctxt = fun name ctxt ->
    let (prefix, suffix) = split_name name in
    let (suffix, ctxt) = get_suffix prefix suffix ctxt in
    (merge_name prefix suffix, ctxt)

  let reserve_name : string -> ctxt -> ctxt = fun name ctxt ->
    let (prefix, suffix) = split_name name in
    try
      let n = SMap.find prefix ctxt in
      if suffix <= n then ctxt else SMap.add prefix suffix ctxt
    with Not_found -> SMap.add prefix suffix ctxt
end)
