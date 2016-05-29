open Basic
open Bindlib
open Genlex
open Format
open Filename
open Sys
open Parser

let treat_exc fn a =
  try
    fn a
  with
    | Decap.Parse_error (s,l,c,l',c') ->
        print_string "*** Syntax error: ";
	print_string s;
        print_newline()
    | Unbound s ->
        print_string "*** Unbound variable: ";
	print_string s;
        print_newline()
    | Failure s ->
        print_newline();
        print_string "*** Failed: "; print_string s; print_newline()
    | Sys_error s ->
        print_newline();
        print_string "*** System error: "; print_string s; print_newline()
    | Invalid_argument s ->
        print_newline();
        print_string "*** Invalid_argument: "; print_string s;
        print_newline()
    | Break ->
        print_newline();
        print_string "*** User interupt"; print_newline()
    | Exit ->
        print_newline();
        print_string "*** Exit"; print_newline()
    | Not_found ->
        print_newline();
        print_string "*** Not_found"; print_newline()
    | Out_of_memory ->
        print_newline();
        print_string "*** Out of memory"; print_newline()
    | End_of_file ->
       exit 0

let _ =
  catch_break true;
  for i = 1 to Array.length Sys.argv - 1 do
    treat_exc (read_file parse_cmds) Sys.argv.(i);
  done;
  while true do
    Printf.printf "reading standard input\n%!";
    treat_exc (Decap.parse_channel parse_cmds blank) stdin
  done
