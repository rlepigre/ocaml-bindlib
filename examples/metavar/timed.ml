(****************************************************************************)
(**{3                        Undoable references                           }*)
(****************************************************************************)

(** This module provides alternative functions for updating references
    (that is, terms of type ['a ref]) and enables the restoration of a
    previously saved state by "undoing" the updates. *)

module Time =
(** [Time] submodule allows to [save] the current time and [rollback]
    the references. If the time is not accessible.

    old values are collected by the GC if no time are accessible
    that would allow to rollback to this value.

    TODO: Innacessible value after an accessible time are not
    collected.
*)
  struct
    type t = { mutable next : t option ; undo : unit -> unit }

    let current : t ref =
      ref { next = None ; undo = (fun () -> ()) }

    let save : unit -> t = fun () -> !current

    let rollback : t -> unit = fun t ->
      let rec fn = function
        | None   -> ()
        | Some t -> fn t.next; t.undo (); t.next <- None
      in fn t.next; t.next <- None; current := t
  end

(** equivalent to Pervasives.(:=) *)
let (:=) : 'a ref -> 'a -> unit = fun r v ->
  let open Time in
  let v0 = !r in
  let t = { next = None; undo = (fun () -> r := v0) } in
  !current.next <- Some t; current := t; r := v

(** equivalent to Pervasives.incr *)
let incr : int ref -> unit = fun r -> r := !r + 1

(** equivalent to Pervasives.decr *)
let decr : int ref -> unit = fun r -> r := !r - 1

(** apply a function and always rollback the pointers *)
let pure_apply : ('a -> 'b) -> 'a -> 'b = fun f v ->
  let t = Time.save () in
  try
    let r = f v in
    Time.rollback t; r
  with e ->
    Time.rollback t; raise e

(** apply a test and rollback the pointers if the test
    returns false or raises an exception *)
let pure_test : ('a -> bool) -> 'a -> bool = fun f v ->
  let t = Time.save () in
  try
    let r = f v in
    if not r then Time.rollback t; r
  with e ->
    Time.rollback t; raise e
