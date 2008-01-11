open Basic
open Bindlib
open Genlex
open File
open Action
open Globals
open Print
open Stream 
open Reduce
open Format

type meta_lvl =
    Nometa 
  | Meta_left of int * 
	(term, term) mbinder array bindbox * 
	(string * (int * int)) list
  | Meta_right of (term, term) mbinder array bindbox * 
	(string * (int * int)) list

let keywords = [
  "("; ")"; "["; "]"; "."; ";"; "let"; "load"; "cst"; "red"; ">>";
  "dump"; "show"; "prn" ]

let lexer = make_lexer keywords

let stream_check f s = 
  match Stream.peek s with
    None -> raise Stream.Failure
  | Some x -> if f x then Stream.next s else raise Stream.Failure

let is_infix lvl_left lvl_top = function
  Ident s -> (
    try 
      let sym = lookup s in
      match sym.symbol_fix with
	Prefix -> false
      |	Infix (assoc) ->
	  let lvl = if assoc = Left then sym.symbol_priority 
	  else sym.symbol_priority +. epsilon in
          lvl_left >= lvl & lvl >= lvl_top
    with
      Not_found -> false)
| _ -> false

let parse_infix lvl_left lvl_top strm =
  match (stream_check (is_infix lvl_left lvl_top) strm) with 
    Ident str -> lookup str
  | _ ->  failwith "bug in parse_ident"

let is_prefix lvl_top = function
  Ident s -> (
    try 
      let sym = lookup s in
      sym.symbol_fix = Prefix (* & sym.symbol_priority >= lvl_top *)
    with
      Not_found -> false)
| _ -> false

let parse_prefix lvl_top strm =
  match (stream_check (is_prefix lvl_top) strm) with 
    Ident str -> lookup str
  | _ ->  failwith "bug in parse_ident"

let is_ident = function
  Ident s -> (
    try 
      ignore (lookup s); false
    with
      Not_found ->
    	match s.[0] with
	  'a'..'z' | 'A'..'Z' -> true
	|  _ -> false)
| _ -> false

let parse_ident s = 
  match (stream_check is_ident s) with 
    Ident str -> str
  | _ ->  failwith "bug in parse_ident"

let metas = ref Nometa

let rec parse_list fn = parser
    [< s = fn; l = parse_list' fn >] -> s::l
  | [<>] -> []
							
and parse_list' fn = parser
    [< 'Kwd ","; l = parse_list fn >] -> l 
  | [< >] -> []
      
let parse_ident_list = parse_list parse_ident

let rec parse_atom lvl env = parser
    [< name = parse_ident; t = parse_bind name lvl env >] -> t
  | [< 'Kwd "("; t = parse_expr' min_priority env; 'Kwd ")" >] -> t

and parse_bind name lvl env = parser
      [< 'Kwd "."; strm >] ->
				let f = bind var x as name in parse_expr' bind_lvl ((name,x)::env) strm in
				Bind(^ (^is_closed f^), (^name^), f^)
     | [< 'Kwd "["; str >] -> (
	match !metas with
	  Nometa -> raise (Stream.Error "Meta variables not allowed")
	| Meta_right (ma, lm) -> (
	    match str with parser
	      [< l = parse_list (parse_expr' min_priority env); 'Kwd "]" >] ->
            	let ar, index = try 
		  List.assoc name lm
	    	with Not_found ->
                  raise (Unbound name)
		in
		if ar <> List.length l then 
		  raise (Stream.Error "Arity mismatch");
		bmeta index ma (Array.of_list l)
	    )
	| Meta_left (cur, ma, lm) -> 
	    match str with parser
	      [< l = parse_ident_list; 'Kwd "]" >] ->
		let l = List.map (find_local env) l in
            	let index = try 
		  let _ = List.assoc name lm in
		  raise (Stream.Error "Non linear pattern")
	    	with Not_found ->
		  metas := 
		     Meta_left (cur+1, ma, 
				(name,(List.length l, cur))::lm);
		  cur
		in
         UVar(^ ma, (^index^),  (lift_array (Array.of_list l)) ^)
	    )
    | [< >] -> find_name env name

and parse_prefix_args sym env strm = 
  let n = sym.symbol_arity in
  let args = Array.create n (unit Dummy) in
  parse_prefix_args' args sym.symbol_priority env n strm;
	let args = lift_array args in 
  App(^ (^is_closed args^), (^false^), (^sym^), args ^)

and parse_prefix_args' args lvl env = function
    0 -> (fun _ -> ())
  | n -> parser
	[< t = parse_expr' lvl env; 
	  _ = parse_prefix_args' args lvl env (n-1) >] -> 
	  args.(Array.length args - n) <- t

and parse_app lvl_left lvl_top strm =
  if lvl_left >= app_lvl & app_lvl >= lvl_top then sapp
  else raise Stream.Failure

and parse_infix_suit t lvl_left lvl_top env = parser
    [< sym = parse_infix lvl_left lvl_top; 
       t' = parse_infix_suit' t sym lvl_top env >] -> t' 
  | [< sym = parse_app lvl_left lvl_top; 
       t' = parse_app_suit t sym lvl_top env >] -> t'
  | [< >] -> t

and parse_app_suit t sym lvl_top env = parser
  [< t' = parse_infix_suit' t sym lvl_top env >] -> t'
| [< >] -> t

and parse_infix_suit' t sym lvl_top env strm = 
  if sym.symbol_arity = 1 then
		let args = [|^ t ^|] in 
    let tr = App(^ (^is_closed args^), (^false^), (^sym^), args^) in 
    parse_infix_suit tr sym.symbol_priority lvl_top env strm
  else
    let lvl = 
      match sym.symbol_fix with
	Infix(Right) -> sym.symbol_priority
      |	_ -> sym.symbol_priority +. epsilon
    in 
    let t' = parse_expr' lvl env strm in
	  let args = [|^ t;t' ^|] in 
    let tr = App(^ (^is_closed args^), (^false^), (^sym^), args^) in 
    parse_infix_suit tr sym.symbol_priority lvl_top env strm
    
and parse_expr' lvl env = parser
    [< sym = parse_prefix lvl; t = parse_prefix_args sym env;
        t' = parse_infix_suit t max_priority lvl env >] -> t'
  | [< t = parse_atom lvl env; 
      t' = parse_infix_suit t max_priority lvl env >] -> t'

and parse_expr strm =
  unbox (parse_expr' min_priority [] strm)

let parse_syntax = parser
    [< 'Ident "Prefix" >] -> Prefix
  | [< 'Ident "Infix" >] -> Infix(NonAssoc)
  | [< 'Ident "LInfix" >] -> Infix(Left)
  | [< 'Ident "RInfix" >] -> Infix(Right)
  

let parse_red strm =
  try
    let pat =
      unbox (bind var ma in 
      	metas := Meta_left(0, ma, []);
      	parse_expr' min_priority [] strm) in
        let Meta_left(num, _, lars) = !metas in
    let larv = Array.of_list (List.map (fun (_,(ar,_)) -> ar) lars) in
    (match strm with parser [< 'Kwd ">>" >] -> ());
    let res =
      unbox (bind var ma in 
      	metas := Meta_right(ma, lars);
      	parse_expr' min_priority [] strm) in
    metas := Nometa;
    larv, pat, res
  with e -> 
    metas := Nometa; 
    raise e
  
let rec parse_cmd = parser
    [< 'Kwd "let"; 'Ident name; 'Ident "="; e = parse_expr >] ->
	define name e

  | [< e = parse_expr; 'Kwd ";" >] ->
      print_term (reduce e);
      print_newline ()
	   
  | [< 'Kwd "cst"; 'Ident name; 'Int arity; fix = parse_syntax; 
      'Float priority; 'Kwd ";" >] ->
      add_cst name arity fix priority

  | [< 'Kwd "red"; pat = parse_red; 'Kwd ";" >] ->
      add_red pat

  | [< 'Kwd "prn";  e = parse_expr; 'Kwd ";" >] ->
      do_prn e

  | [< 'Kwd "show"; 'Ident name; 'Kwd ";" >] ->
      do_show name

  | [< 'Kwd "dump"; 'Kwd ";" >] ->
      do_show_all ()

  | [< 'Kwd "load"; 'String s; 'Kwd ";" >] ->
      read_file s

  | [< 'Kwd "EOF" >] ->
      pop_input ()

