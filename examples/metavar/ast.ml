(** Ast constructed by the parser. *)

(** Parsed term. *)
type pterm =
  | PApp of pterm * pterm
  (** Application. *)
  | PLam of string * pterm
  (** Abstraction. *)
  | PVar of string
  (** Variable. *)
  | PNrm of pterm
  (** Normalisation of a term. *)

let many_PLam : string list -> pterm -> pterm =
  List.fold_right (fun x t -> PLam(x,t))

(** Parsed command. *)
type cmd =
  | Decl of string * pterm
  (** Declare a meta-variable. *)
  | Prnt of pterm
  (** Print the normal form of the given term. *)
  | Undo
  (** Undo the previous command (excluding [Undo]). *)
  | Goal
  (** Print all the goals (unset meta-variables). *)
