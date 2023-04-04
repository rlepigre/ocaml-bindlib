(* This code is largely inspired by the implementation of [Set] from the OCaml
   standard library. *)

(* A set of integers is represented as a set of intervals stored in a balanced
   binary search tree. The intervals are always maximal, which means that when
   [Node({l; vl; vr; r; h})] appers in the representation of a set, then it is
   guaranteed that [vl-1] and [vr+1] are not in the set. *)
type t =
  | Leaf
  | Node of {l:t; vl:int; vr:int; r:t; h:int}

let height t = match t with Leaf -> 0 | Node({h;_}) -> h

let node l vl vr r = Node({l; vl; vr; r; h = max (height l) (height r) + 1})

let rec mem : int -> t -> bool = fun i t ->
  match t with
  | Leaf                         -> false
  | Node({l; vl; _}) when i < vl -> mem i l
  | Node({vr; r; _}) when vr < i -> mem i r
  | Node(_)                      -> true

let empty : t =
  Leaf

let is_empty : t -> bool = fun t ->
  t = Leaf

let singleton : int -> t = fun i ->
  Node({l = Leaf; vl = i; vr = i; r = Leaf; h = 1})

let balance : t -> int -> int -> t -> t = fun l vl vr r ->
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Leaf                                      -> assert false
    | Node({l=ll ; vl=lvl ; vr=lvr ; r=lr ; _}) ->
    if height ll >= height lr then node ll lvl lvr (node lr vl vr r) else
    match lr with
    | Leaf                                      -> assert false
    | Node({l=lrl; vl=lrvl; vr=lrvr; r=lrr; _}) ->
    node (node ll lvl lvr lrl) lrvl lrvr (node lrr vl vr r)
  else if hr > hl + 2 then
    match r with
    | Leaf                                      -> assert false
    | Node({l=rl ; vl=rvl ; vr=rvr ; r=rr ; _}) ->
    if height rr >= height rl then node (node l vl vr rl) rvl rvr rr else
    match rl with
    | Leaf                                      -> assert false
    | Node({l=rll; vl=rlvl; vr=rlvr; r=rlr; _}) ->
    node (node l vl vr rll) rlvl rlvr (node rlr rvl rvr rr)
  else
    node l vl vr r

let rec min_elt : t -> int = fun t ->
  match t with
  | Leaf                    -> raise Not_found
  | Node({l=Leaf; vl  ; _}) -> vl
  | Node({l     ; vl=_; _}) -> min_elt l

let min_elt_opt : t -> int option = fun s ->
  try Some(min_elt s) with Not_found -> None

let rec max_elt : t -> int = fun t ->
  match t with
  | Leaf                    -> raise Not_found
  | Node({vr  ; r=Leaf; _}) -> vr
  | Node({vr=_; r     ; _}) -> max_elt r

let max_elt_opt : t -> int option = fun s ->
  try Some(max_elt s) with Not_found -> None

let rec set_min_elt : t -> int -> t = fun t i ->
  match t with
  | Leaf                         -> assert false
  | Node({l; vl; vr; r; h} as d) ->
  match l with
  | Leaf    -> Node({d with vl = i})
  | Node(_) -> Node({l = set_min_elt l i; vl; vr; r; h})

let rec set_max_elt : t -> int -> t = fun t i ->
  match t with
  | Leaf                         -> assert false
  | Node({l; vl; vr; r; h} as d) ->
  match r with
  | Leaf    -> Node({d with vr = i})
  | Node(_) -> Node({l; vl; vr; r = set_max_elt r i; h})

let rec min_interval : t -> int * int = fun t ->
  match t with
  | Leaf                          -> raise Not_found
  | Node({l=Leaf; vl  ; vr  ; _}) -> (vl, vr)
  | Node({l     ; vl=_; vr=_; _}) -> min_interval l

let rec max_interval : t -> int * int = fun t ->
  match t with
  | Leaf                          -> raise Not_found
  | Node({vl  ; vr  ; r=Leaf; _}) -> (vl, vr)
  | Node({vl=_; vr=_; r     ; _}) -> max_interval r

let rec remove_min_interval : t -> t = fun t ->
  match t with
  | Leaf                             -> assert false
  | Node({l=Leaf; vl=_; vr=_; r; _}) -> r
  | Node({l     ; vl  ; vr  ; r; _}) ->
      balance (remove_min_interval l) vl vr r

let merge : t -> t -> t = fun l r ->
  match (l, r) with
  | (Leaf, t   ) -> t
  | (t   , Leaf) -> t
  | (_   , _   ) ->
      let (vl, vr) = min_interval r in
      balance l vl vr (remove_min_interval r)

let rec add : int -> t -> t = fun i t ->
  match t with
  | Leaf                                -> singleton i
  | Node({l; vl; vr; r; h}) when i < vl ->
      if i = vl - 1 then
        (* The current segment needs to be merged. *)
        if max_elt_opt l = Some(vl - 2) then
          merge (set_max_elt l vr) r
        else
          Node({l; vl=i; vr; r; h})
      else
        (* Insert in the left subtree. *)
        let new_l = add i l in
        if new_l == l then t else balance new_l vl vr r
  | Node({l; vl; vr; r; h}) when vr < i ->
      if i = vr + 1 then
        (* The current segment needs to be merged. *)
        if min_elt_opt r = Some(vr + 2) then
          merge l (set_min_elt r vl)
        else
          Node({l; vl; vr=i; r; h})
      else
        (* Insert in the right subtree. *)
        let new_r = add i r in
        if new_r == r then t else balance l vl vr new_r
  | Node(_)                             -> t

let from_list : int list -> t =
  List.fold_left (fun t i -> add i t) empty

let rec iter_intervals : (int -> int -> unit) -> t -> unit = fun f s ->
  match s with
  | Leaf                    -> ()
  | Node({l; vl; vr; r; _}) -> iter_intervals f l; f vl vr; iter_intervals f r

let iter : (int -> unit) -> t -> unit = fun f s ->
  let f lb ub = for i = lb to ub do f i done in
  iter_intervals f s

let fold : (int -> 'a -> 'a) -> t -> 'a -> 'a = fun f s e ->
  let acc = ref e in
  iter (fun i -> acc := f i !acc) s; !acc

let fold_intervals : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a = fun f s e ->
  let acc = ref e in
  iter_intervals (fun lb ub -> acc := f lb ub !acc) s; !acc

let smallest_not_in_above : int -> t -> int = fun lb s ->
  let succ i = if i < max_int then i + 1 else raise Not_found in
  let rec find s =
    match s with
    | Node({l=_; vl=_; vr  ; r  ; _}) when vr < lb     -> find r
    | Node({l  ; vl  ; vr=_; r=_; _}) when lb < vl - 1 -> find l
    | Node({l=_; vl=_; vr  ; r=_; _})                  -> succ vr
    | Leaf                                             -> succ lb
  in
  find s

let largest_not_in_below : int -> t -> int = fun ub s ->
  let pred i = if i > min_int then i - 1 else raise Not_found in
  let rec find s =
    match s with
    | Node({l  ; vl  ; vr=_; r=_; _}) when ub < vl     -> find l
    | Node({l=_; vl=_; vr  ; r  ; _}) when vr + 1 < ub -> find r
    | Node({l=_; vl  ; vr=_; r=_; _})                  -> pred vl
    | Leaf                                             -> pred ub
  in
  find s

(*
let debug_print t =
  let rec print pad t =
    match t with
    | Leaf                    -> Printf.printf "%s∅\n" pad
    | Node({l; vl; vr; r; _}) ->
        Printf.printf "%s[%i; %i]\n" pad vl vr;
        print (pad ^ " ") l;
        print (pad ^ " ") r
  in
  print "" t;
  Printf.printf "%!"

let _ =
  let s = from_list [1; 3; 4; 5; 7; 9; 2] in
  debug_print s;
  iter_intervals (fun i j -> Printf.printf "[%i, %i] " i j) s;
  Printf.printf "\n%!";
  for i = -2 to 12 do
    let j = smallest_not_in_above i s in
    Printf.printf "smallest_not_in_above %i s = %i\n%!" i j
  done;
  for i = -2 to 12 do
    let j = largest_not_in_below i s in
    Printf.printf "largest_not_in_below %i s = %i\n%!" i j
  done
*)
