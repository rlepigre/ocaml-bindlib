(** Type of a set of integers, represented as a collection of intervals. *)
type t

(** [empty] is an empty set of integers. *)
val empty : t

(** [is_empty s] tests whether the set [s] is empty. *)
val is_empty : t -> bool

(** [mem i s] tests whether the integer [i] is contained in the set [s]. *)
val mem : int -> t -> bool

(** [add i s] returns a set that extends [s] with [i]. If [s] already contains
    [i] then the returned set is physically equal to [s]. *)
val add : int -> t -> t

(** [singleton i] returns a set containing the integer [i] only. *)
val singleton : int -> t

(** [min_elt s] gives the smallest integer contained in the set [s], or raises
    the [Not_found] exception if [s] is empty. *)
val min_elt : t -> int

(** [max_elt s] gives the largest integer contained in the set [s],  or raises
    the [Not_found] exception if [s] is empty. *)
val max_elt : t -> int

(** [min_interval s] gives a pair [(lb,ub)] representing the smallest interval
    contained in the set [s], and that includes [min s]. Note that [lb <= ub],
    and that [lb = min s]. *)
val min_interval : t -> int * int

(** [max_interval s] gives a pair [(lb, ub)] representing the largest interval
    contained in the set [s], and that includes [max s]. Note that [lb <= ub],
    and that [ub = max s]. *)
val max_interval : t -> int * int

(** [from_list l] returns a set containing the elements of the list [l]. *)
val from_list : int list -> t

(** [iter f s] calls [f] on all the elements of the set [s] in ascending order
    (i.e., from the lowest to the largest element). *)
val iter : (int -> unit) -> t -> unit

(** [iter_intervals f s] calls [f lb ub] on every interval [(lb,ub)] contained
    in the representation of [s]. Such intervals are as large as possible, and
    disjoint. They are traversed in ascending order. *)
val iter_intervals : (int -> int -> unit) -> t -> unit

(** [fold f s e] folds function [f] over the elements of set [s], using [e] as
    the initial value of the accumulator. The elements of [s] are traversed in
    ascending order. *)
val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

(** [fold_intervals f s e] folds function [f] over the intervals contained  in
    the representation of set [s] (see [iter_intervals]), using [e] as initial
    accumulator. The intervals are visited in ascending order. *)
val fold_intervals : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

(** [smallest_not_in_above lb s] returns the smallest integer not contained in
    [s] that is strictly greater than [lb]. If there is no such element,  then
    the [Not_found] exception is raised. *)
val smallest_not_in_above : int -> t -> int

(** [largest_not_in_below ub s] returns the largest integer not in [s] that is
    strictly smaller than [ub]. If there is no such element,  then [Not_found]
    is raised. *)
val largest_not_in_below : int -> t -> int
