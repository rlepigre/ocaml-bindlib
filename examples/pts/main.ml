open Basic
open Bindlib
open Genlex
open Format
open Filename
open Sys
open Parser
open File

module Make(Pts: PtsType) =
  struct
    open Pts
    module ParserPts = Parser.Make(Pts)
    open ParserPts
    open ActionPts
    open Type_checkPts
    open PrintPts
    open GlobalsPts
    open BasicPts

    let main() =
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
	    print_string s;
            resetlex strc;
            print_newline()
      	|	Unbound s ->
            print_pos ();
            print_string "*** Unbound variable: ";
	    print_string s;
            resetlex strc;
            print_newline()
      	|	Stream.Failure ->
            print_pos ();
            print_string "*** Syntax error";
            resetlex strc;
            print_newline()
      	|	Ill_axiom s ->
            print_pos ();
            print_string "*** No axiom starting with ";
	    print_sort s; 
            print_newline()
      	|	Ill_rule (s1,s2) ->
            print_pos ();
            print_string "*** No rule starting with ";
	    print_sort s1;
	    print_string ",";
	    print_sort s2; 
            print_newline()
      	|	Ill_sort (e) ->
            print_pos ();
	    open_hovbox 0;
            print_string "*** Can not infer sort of";
	    print_break 1 2;
	    print_expr e;
	    close_box ();
            print_newline()
      	|	Ill_type (e,t) ->
            print_pos ();
	    open_hovbox 0;
            print_string "*** Type mismatch ";
	    print_break 1 2;
	    print_expr e;
	    print_string " :";
	    print_break 1 2;
	    print_expr t; 
	    close_box ();
            print_newline()
      	|	Mismatch (e,e') ->
            print_pos ();
	    open_hovbox 0;
            print_string "*** Convertibility mismatch:";
	    print_break 1 2;
	    print_expr e;
	    print_string " =";
	    print_break 1 2;
	    print_expr e'; 
	    close_box ();
            print_newline()
      	| Failure s ->
            print_newline();
            print_string "*** Failed: "; print_string s; print_newline()
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
            print_pos ();
            print_newline();
            print_string "*** Unexpected end of file"; print_newline()
    	| Sys_error s ->
            print_newline();
            print_string "*** System error: "; print_string s; print_newline()
       	| Quit -> 
	    print_endline " Bye";
	    exit 0
      done
  end
