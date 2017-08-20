open Basic
open Bindlib
open Genlex
open Format

let blank = EarleyStr.blank_regexp ''\([ \n\r\t]\|\(//[^\n]*\n\)\)*''

let read_file parse_cmds filename =
  let ch = open_in filename in
  Printf.printf "reading %s\n%!" filename;
  try Earley.parse_channel parse_cmds blank ch
  with End_of_file -> ()

module Make(Pts: PtsType) =
  struct
    open Pts
    module ActionPts = Action.Make(Pts)
    open ActionPts
    open Type_checkPts
    open PrintPts
    open GlobalsPts
    open BasicPts

    let rec build_binder l f env t e =
      match l with
	[] -> e env
      |	name::l -> f name t (fun x ->
	build_binder l f (add_local env name x) t e)

    let reserved = [ "let"; "load"; "quit" ]
    let parser ident =
      | s:''[a-zA-Z_][a-zA-Z_0-9]*'' ->
	if List.mem s reserved then Earley.give_up (); s

    let parser string =
      | '"' - s:''[^"]*'' - '"' -> s

    let parser parse_ident_list =
      	s:ident l:{_:',' l:ident}*$ -> s::l

    let parser parse_expr_list =
      e:parse_expr' l:{ ',' e':parse_expr'}*$ ->
        (fun env -> List.map (fun e -> e env) (e::l))

    and parse_atom =
        name:ident -> (fun env -> find_name env name)

      | '(' e:parse_expr' ')' -> e

      |	'[' l:parse_expr_list ']' ->
	 (fun env -> match List.rev (l env) with
	   [] -> assert false
	 | t::l -> List.fold_left (fun acc t' -> pair t' acc) t l)

      | s:parse_sort -> (fun env -> box (Atom s))

      | '{' s:string '}' -> (fun env -> box (Goal s))

    and parse_proj =
      |	t:parse_atom -> t
      |	t:parse_proj ".1" -> (fun env -> proj1 (t env))
      |	t:parse_proj ".2" -> (fun env -> proj2 (t env))

    and parse_app =
      | t:parse_proj -> t
      |	t:parse_app t':parse_proj -> (fun env -> app (t env) (t' env))

    and parse_arrow =
      | {"->"|"→"} t':parse_expr' -> (fun env t -> pi "_" t (fun _ -> (t' env)))
      | {'&'|"×"|"∧"}  t':parse_expr' -> (fun env t -> sigma "_" t (fun _ -> (t' env)))
      |	EMPTY -> (fun env t -> t)

    and parse_expr' =
      | {"∀"|"Π"} l:parse_ident_list ':' t:parse_expr' '.' e:parse_expr' ->
	 (fun env -> build_binder l pi env (t env) e)
      | {"∃"|"Σ"} l:parse_ident_list ':' t:parse_expr' '.' e:parse_expr' ->
	 (fun env -> build_binder l sigma env (t env) e)
      |	{"λ"|"Λ"} l:parse_ident_list ':' t:parse_expr' '.' e:parse_expr' ->
	 (fun env -> build_binder l lambda env (t env) e)
      | t:parse_app t':parse_arrow ->
 	 (fun env -> t' env (t env))

    and parse_expr =
      | e:parse_expr' -> unbox (e [])


    let parser parse_def =
      | '=' e:parse_expr ':' t:parse_expr ->
	 (fun name -> ignore(define name e t))
      | ':' t:parse_expr '=' e:parse_expr ->
	 (fun name -> ignore(define name e t))

    let parser parse_cmd =
      | "let" name:ident d:parse_def -> d name

      |	e:parse_expr ':' t:parse_expr ->
	  type_check e t;
	  print_expr (normalize e);
	  print_newline ()

      |	"load" s:string -> read_file parse_cmds s

      | "quit" -> exit 0

      | EOF -> raise End_of_file

    and parse_cmds = _:{parse_cmd ';'}*

  end
