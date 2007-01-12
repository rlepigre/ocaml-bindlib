open Basic
open Bindlib
open Format

let genname name nv = 
  let name = 
    if not (StringSet.mem name nv) then name
    else
      let i = ref 0 in
      let namex = ref (name ^ string_of_int !i) in
      while StringSet.mem !namex nv do
	incr i;
	namex := name ^ string_of_int !i
      done; 
      !namex
  in name, StringSet.add name nv

let max_priority = 1e100
let min_priority = -1e100
let epsilon = 10e-8
let bind_lvl = 0.0
let app_lvl = 10.0

let print_term, print_term' = 
  let rec fn lvl nv = function
      Bind(_,name,f) ->    
	if lvl > bind_lvl then print_string "(";
	if lvl <> bind_lvl then open_hbox ();
      	let name, nv = genname name nv in 
	match f with bind var x in t ->
      	print_string name; 
      	print_string ".";
      	fn bind_lvl nv t;
	if lvl <> bind_lvl then close_box ();
	if lvl > bind_lvl then print_string ")"
    | App(_, _, s, ts) -> 
	if s.symbol_arity = 0 then print_string s.symbol_name else begin
 	  if lvl > s.symbol_priority then print_string "(";
	  if lvl <> s.symbol_priority then open_hovbox 2;
	  begin
	    match s.symbol_fix with
	      Prefix ->
     	      	print_string s.symbol_name;
      	      	if s.symbol_arity > 0 then begin
      	      	  for i = 0 to s.symbol_arity - 1 do
	      	    print_space ();
	    	    fn s.symbol_priority nv ts.(i)
      	      	  done
      	      	end;
	    | Infix(assoc) ->
	      	let lvl_left = 
	      	  if assoc = Left then s.symbol_priority 
		  else s.symbol_priority +. epsilon
	      	and lvl_right =
	      	  if assoc = Right then s.symbol_priority 
		  else s.symbol_priority +. epsilon
	      	in
              	match s.symbol_arity with
		  1 -> 
		    fn lvl_left nv ts.(0);
		    print_space ();
	     	    print_string s.symbol_name;
	      	| 2 ->
		    fn lvl_left nv ts.(0);
		    print_space ();
	     	    if s.symbol_name <> "" then begin
		      print_string s.symbol_name;
		      print_space ()
		    end;
		    fn lvl_right nv ts.(1);
	      	| _ ->
		    failwith "bug in print_term"
	  end;
	  if lvl <> s.symbol_priority then close_box ();
	  if lvl > s.symbol_priority then print_string ")"
	end
 
    | Var v ->
      	print_string (name_of v)
    | Def(name,_,_) ->
      	print_string name
    | UVar(_, index, tl) ->
      	print_string "X";
 	print_int index;
	print_string "[";
	let rec gn = function
	    [] -> ()
	  | [t] -> fn min_priority nv t;
	  | t::suit -> fn min_priority nv t; print_string ","; gn suit
        in gn tl;
	print_string "]"
    | Varint (i) ->
      	print_string "$"; print_int i
    | Dummy ->
      	print_string "!!!"

in fn min_priority StringSet.empty, fn min_priority 

let print_pattern (lars, pat, res) =
  let nv = ref StringSet.empty in
  let vars = Array.mapi (fun n _ ->
    let name = "X"^string_of_int n in
    nv := StringSet.add name !nv;
    unbox (bind lvar tl in UVar (^ [|^ ^|], (^n^),  tl^))) lars
  in
  open_hovbox 2;
  print_term' !nv (subst pat vars);
  print_space ();
  print_string ">>";
  print_space ();
  print_term' !nv (subst res vars);
  close_box ();
  print_break 1 0

let print_constants sym =
  open_hovbox 2;
  print_string sym.symbol_name;
  print_space ();
  print_int sym.symbol_arity;
  print_space ();
  (match sym.symbol_fix with
    Prefix -> print_string "Prefix"
  | Infix(NonAssoc) -> print_string "Infix"
  | Infix(Left) -> print_string "LInfix"
  | Infix(Right) -> print_string "RInfix");
  print_space ();
  print_float sym.symbol_priority;
  if sym.symbol_rewrite <> [] then begin
    force_newline ();
    open_vbox 0; 
    List.iter print_pattern sym.symbol_rewrite;
    close_box ();
  end;
  print_newline ()







