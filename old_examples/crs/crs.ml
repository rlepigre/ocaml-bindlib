open Basic
open Bindlib
open Genlex
open Format
open Filename
open Sys
open Parser
open File

let _ = 
  catch_break true;
  let strc = Stream.from read_fun in
  while true do
    try
      let str = lexer strc in
      parse_cmd str; ()
    with
      Stream.Error s ->
        print_pos ();
        print_string "*** Syntax error: ";
        resetlex strc;
	print_string s;
        print_newline()
    | Unbound s ->
        print_pos ();
        print_string "*** Unbound variable: ";
        resetlex strc;
	print_string s;
        print_newline()
    | Stream.Failure ->
        print_pos ();
        print_string "*** Syntax error";
        resetlex strc;
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
    | Bindlib_error ->
        print_newline();
        print_string "*** Bindlib_error"; print_newline()
    | End_of_file ->
        print_pos ();
        print_newline();
        print_string "*** Unexpected end of file"; print_newline()
    | Quit -> 
	print_endline " Bye";
	exit 0
  done
