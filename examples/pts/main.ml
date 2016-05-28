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
      for i = 1 to Array.length Sys.argv - 1 do
	read_file parse_cmds Sys.argv.(i);
      done;
      while true do
	try
	  Decap.parse_channel parse_cmds blank stdin
      	with
	| Exit -> exit 0
       	| Decap.Parse_error(s,l,c,l',c') ->
            print_string "*** Syntax error: ";
	    print_string s;
            print_newline()
      	| Unbound s ->
            print_string "*** Unbound variable: ";
	    print_string s;
            print_newline()
      	|	Stream.Failure ->
            print_string "*** Syntax error";
            print_newline()
      	|	Ill_axiom s ->
            print_string "*** No axiom starting with ";
	    print_sort s;
            print_newline()
      	|	Ill_rule (s1,s2) ->
            print_string "*** No rule starting with ";
	    print_sort s1;
	    print_string ",";
	    print_sort s2;
            print_newline()
      	|	Ill_sort (e) ->
	    open_hovbox 0;
            print_string "*** Can not infer sort of";
	    print_break 1 2;
	    print_expr e;
	    close_box ();
            print_newline()
      	|	Ill_type (e,t) ->
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
      	| Not_found ->
            print_newline();
            print_string "*** Not_found"; print_newline()
      	| Out_of_memory ->
            print_newline();
            print_string "*** Out of memory"; print_newline()
      	| End_of_file ->
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
