open Genlex
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

let test_console () = !stack_input = []

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


let read_file filename = 
    let oin = !cur_input in
    cur_input := (open_path filename);
    stack_input := (oin,!cur_line,!cur_col,!cur_name)::!stack_input;
    pv_col := 0;
    pv_lin := 0;
    cur_line := 1;
    cur_col := 0;
    cur_name := filename;
    print_string "Opening file \"";
    print_string !cur_name;
    print_string "\".";
    print_newline ()


let prompt = "%> "

let print_prompt() =
  print_string prompt; print_flush ()

let rec read_fun _ =
  if !bol & test_console () then print_prompt();
  let c = input_char !cur_input in
  pv_col:=!cur_col;
  pv_lin:=!cur_line;
  incr cur_col;
  bol := c == '\n';
  if !bol then begin incr cur_line; cur_col := 0 end;
  if c == '\013' then cur_col := 0;
  Some c

let print_pos () =
  if not (test_console ()) then begin 
    let l = !cur_line and c = !cur_col and n = !cur_name in
    pop_all ();
    print_string "*** File \"";
    print_string n;
    print_string "\", line ";
    print_int l;
    print_string "\", column ";
    print_int c;
    print_endline " :"
  end
      
let rec resetlex = parser
    [< '(**) '\n' >] -> ()
  | [< '(**) '\r' >] -> ()
  | [< 'c; str  >] -> resetlex str
  | [< >] -> ()



