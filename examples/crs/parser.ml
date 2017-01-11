open Basic
open Bindlib
open Action
open Globals
open Print
open Stream
open Reduce
open Format

let keywords = [
  "."; ";"; "let"; "load"; "cst"; "red"; "->"; "dump"; "show"; "prn"; "quit" ]

let is_infix lvl_left lvl_top s =
    try
      let sym = lookup s in
      match sym.symbol_fix with
	Prefix -> false
      |	Infix (assoc) ->
	  let lvl = if assoc = Left then sym.symbol_priority
	  else sym.symbol_priority +. epsilon in
          lvl_left >= lvl && lvl >= lvl_top
    with
      Not_found -> false

let is_prefix lvl_top s =
    try
      let sym = lookup s in
      sym.symbol_fix = Prefix && sym.symbol_priority >= lvl_top
    with
      Not_found -> false

let is_ident s =
    try
      ignore (lookup s); false
    with
      Not_found ->
    	match s.[0] with
	  'a'..'z' | 'A'..'Z' | '_' -> true
	|  _ -> false

let parser ident =
  | s:''[a-zA-Z_][a-zA-Z0-9_']*'' ->
    if List.mem s keywords then Earley.give_up () else s
  | s:RE("[-=~^@$:+*/<>%&!|\\]+") ->
    if List.mem s keywords then Earley.give_up () else s

let parser int =
  s:''[0-9]+'' -> int_of_string s

let parser string=
  '"' s:''[^"]*'' '"' -> s

let parser float =
  s:''-?[0-9]+\(.[0-9]+\)'' -> float_of_string s

let parser parse_infix (lvl_left, lvl_top) =
  s:ident -> if is_infix lvl_left lvl_top s then lookup s else Earley.give_up ()

let parser parse_prefix lvl_top =
  s:ident -> if is_prefix lvl_top s then lookup s else Earley.give_up ()

let parser parse_ident =
  s:ident -> if is_ident s then s else Earley.give_up ()

let parse_list fn = parser
  | x:fn l:{_:',' x:fn}* -> x::l
  | EMPTY -> []

let parse_ident_list = parse_list parse_ident

let not_bracket =
  let f buf pos =
    let (c,_,_) = Input.read buf pos in
    ((), c <> '[' && c <> '.')
  in
  Earley.test ~name:"no_bracket" Charset.full f

let parser parse_atom lvl =
  | name:parse_ident t:(parse_bind lvl) -> (fun env -> t env name)
  | '(' t:(parse_expr' min_priority) ')' -> t

and parse_bind lvl =
    | '.' t:(parse_expr' bind_lvl)$ when bind_lvl >= lvl -> (fun env name ->
      tbind name (fun x -> t ((name,x)::env)))
    | '['  l:(parse_list (parse_expr' min_priority)) ']' ->
      (fun env name ->
        pvar name (Array.of_list (List.map (fun x -> x env) l)))
    | EMPTY not_bracket -> find_name

and parse_prefix_args sym =
  let n = sym.symbol_arity in
  args:(parse_prefix_args' (sym.symbol_priority, n)) -> (fun env ->
    let args = Array.of_list (List.rev (args env)) in
    app false sym args)

and parse_prefix_args' (lvl,n) =
    | EMPTY when n = 0 -> (fun env -> [])
    | t:(parse_expr' lvl) args:(parse_prefix_args' (lvl,n-1)) when n > 0 ->
       (fun env -> t env::args env)

and parse_infix_suit' (sym, lvl_top) =
  | t':(parse_infix_suit (sym.symbol_priority, lvl_top))
    when sym.symbol_arity = 1 -> (fun env t ->
    let args = [|t|] in
    let tr = app false sym args in
    t' env tr)
  | let lvl =
      match sym.symbol_fix with
	Infix(Right) -> sym.symbol_priority
      |	_ -> sym.symbol_priority +. epsilon
    in
    t':(parse_expr' lvl)
      t'':(parse_infix_suit (sym.symbol_priority, lvl_top)) ->
	   (fun env t ->
	     let args = [|t;t' env|] in
	     let tr = app false sym args in
	     t'' env tr)

and parse_infix_suit (lvl_left, lvl_top) =
    | sym:(parse_infix (lvl_left, lvl_top)) ->>
      t':(parse_infix_suit' (sym, lvl_top)) -> t'
    | t':(parse_infix_suit' (sapp, lvl_top))
	when lvl_left >= app_lvl && app_lvl >= lvl_top -> t'
    | EMPTY -> (fun env t -> t)

and parse_expr' lvl =
  | sym:(parse_prefix lvl) ->> t:(parse_prefix_args sym)
      t':(parse_infix_suit (sym.symbol_priority, lvl)) ->
	  (fun env -> t' env (t env))
  | t:(parse_atom lvl)
      t':(parse_infix_suit (max_priority, lvl)) ->
          (fun env -> t' env (t env))

and parse_expr =
  | t:(parse_expr' min_priority) -> unbox (t [])

let parse_syntax = parser
  | "Prefix" -> Prefix
  | "Infix"  -> Infix(NonAssoc)
  | "LInfix" -> Infix(Left)
  | "RInfix" -> Infix(Right)

let parser parse_red =
  | t1:parse_expr "->" t2:parse_expr ->
   let arities, t1, t2 = bind_reduction t1 t2 in
   arities, t1, t2

let blank = EarleyStr.blank_regexp ''\([ \n\r\t]\|\(//[^\n]*\n\)\)*''

let read_file parse_cmds filename =
  let ch = open_in filename in
  Printf.printf "reading %s\n%!" filename;
  try Earley.parse_channel parse_cmds blank ch
  with End_of_file -> ()

let parser parse_cmd =
  | "let" name:ident '=' e:parse_expr -> define name e

  | "cst" name:ident (arity,fix,priority):{int parse_syntax float | "Atom" -> 0, Prefix, max_priority} ->
      add_cst name arity fix priority

  | "red" pat:parse_red -> add_red pat

  | "prn" e:parse_expr -> do_prn e

  | "show" name:ident -> do_show name

  | "dump" -> do_show_all ()

  | "load" s:string -> read_file parse_cmds s

  | "quit" -> exit 0

  | EOF -> raise End_of_file

  | e:parse_expr ->
      print_term (reduce e);
      print_newline ()

and parse_cmds =
  | {parse_cmd ';'}* -> ()
