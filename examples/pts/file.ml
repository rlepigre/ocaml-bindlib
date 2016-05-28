open Format
open Filename
open Sys

exception Quit

let cur_input = ref stdin
let cur_name = ref "Input from terminal"
let cur_line = ref 1
let cur_col = ref 0
let pv_col = ref (-1)
let pv_lin = ref (-1)

let stack_input = ref ([]:(in_channel * int * int * string) list)

let test_console () =
  !stack_input = []

let start_new = ref true

let bol = ref true

let pop_input0 = function
    (c,l,r,n)::s ->
        print_string "Closing file \"";
        print_string !cur_name;
        print_string "\".";
        print_newline ();
        start_new:=true;
        close_in !cur_input;
        cur_input := c;
        cur_name := n;
        cur_line := l;
        cur_col := r;
        pv_col := r;
        pv_lin := l;
        stack_input := s;
        bol := false;
|    [] ->
        raise Quit


let pop_input () = pop_input0 !stack_input
let pop_all () = while not (test_console ()) do pop_input () done

let path = ref ([] : string list)

let open_path filename =
  try open_in filename
  with Sys_error _ as error ->
    if not (is_implicit filename) then raise error
    else
      let rec fn = function
	    [] -> raise error
	   | dir::l -> try open_in (concat dir filename)
	               with Sys_error _ -> fn l
      in fn !path

let blank = Decap.blank_regexp ''\([ \n\r\t]\|\(//[^\n]*\n\)\)*''

let read_file parse_cmds filename =
  let ch = open_in filename in
  try Decap.parse_channel parse_cmds blank ch
  with Exit -> ()

(*
let prompt = "%> "

let print_prompt() =
  if Unix.((fstat stdin).st_kind = S_CHR) then (
    print_string prompt; print_flush ()
  )
*)
