(** Preprocessor for removing the "[@@unboxed]" attribute on [Bindlib.any_var]
    if the OCaml version is lower that 4.11. *)

let iter_lines : (string -> unit) -> string -> unit = fun f file ->
  let ic = open_in file in
  try while true do f (input_line ic) done with End_of_file -> close_in ic

(** OCaml version given as first command line argument. *)
let version =
  match String.split_on_char '.' Sys.argv.(1) with
  | major :: minor :: _ -> (int_of_string major, int_of_string minor)
  | _                   -> assert false

(** File to process given as second command line argument. *)
let input_file = Sys.argv.(2)

let print_line : string -> unit = fun line ->
  print_string line; print_char '\n'

let print_line_if_not_unbox : string -> unit = fun line ->
  if String.trim line <> "[@@unboxed]" then print_line line

let _ =
  let handle_line =
    if version < (4, 11) then print_line_if_not_unbox else print_line
  in
  iter_lines handle_line input_file
