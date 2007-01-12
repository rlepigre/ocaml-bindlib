open Basic
open Bindlib
open Genlex
open File

module Make(Pts: PtsType) =
  struct
    open Pts
    module ActionPts = Action.Make(Pts)
    open ActionPts
    open Type_checkPts
    open PrintPts
    open GlobalsPts
    open BasicPts

    let keywords = [
      "("; ")"; "{"; "}"; "["; "]"; "?";
      "->"; ":"; "="; ","; ";"; "let"; "load"
    ] @ Pts.keywords

    let lexer = make_lexer keywords

    let rec parse_ident_list = parser
      	[< 'Ident s; l = parse_ident_list' >] -> s::l
						       
    and parse_ident_list' = parser
      	[< 'Kwd ","; l = parse_ident_list >] -> l 
      | [< >] -> []
      
    let rec parse_atom env = parser
        [< 'Ident name >] ->
	  find_name env name
      |	[< 'Kwd "("; t = parse_expr' env; 'Kwd ")" >] ->
	  t
      | [< s = parse_sort >] -> Atom(^unit s^)
	    
    and build_binder l f env str =
      match l with
	[] -> parse_expr' env str
      |	name::l -> 
	  f (bind fVar x as name in 
             build_binder l f (add_local env name x) str)

    and parse_app env = parser
       	[< t = parse_atom env; t' = parse_app' env t >] -> t'

    and parse_app' env t = parser
       	[< t' = parse_atom env; t'' = parse_app' env (App(^t, t'^)) >] -> t''
      |	[< >] -> t
 
    and parse_arrow env t = parser
	[< 'Kwd "->"; t' = parse_expr' env >] -> bArrow t t'
      |	[< >] -> t

    and parse_expr' env = parser
      	[< 'Kwd "{"; l = parse_ident_list; 'Kwd ":"; t = parse_expr' env;
	  'Kwd "}"; str >] -> 
	    build_binder l (fun f -> Pi(^t, f^)) env str
      |	[< 'Kwd "["; l = parse_ident_list; 'Kwd ":"; t = parse_expr' env;
	  'Kwd "]"; str >] -> 
	    build_binder l (fun f -> Lambda(^t, f^)) env str
      | [< t = parse_app env; t' = parse_arrow env t >] ->
 	  t'

    and parse_expr str =
      unbox (parse_expr' [] str)


    let rec parse_cmd = parser
	[< 'Kwd "let"; 'Ident name; 'Kwd "="; e = parse_expr; 'Kwd ":";
           t = parse_expr; 'Kwd ";" >] ->
	     ignore(define name e t)

      |	[< e = parse_expr; 'Kwd ":"; t = parse_expr; 'Kwd ";" >] ->
	  type_check e t;
	  print_expr (normalize e);
	  print_newline ()
	   
      |	[< 'Kwd "load"; 'String s; 'Kwd ";" >] ->
	  read_file s

      |	[< 'Kwd "EOF" >] ->
	  pop_input ()

  end
