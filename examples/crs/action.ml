open Basic
open Bindlib
open Format
open Globals
open Print

exception Ill_tactic of string
	  
let define name e =
  environment := {
     definitions = 
       StringMap.add name (Def(name,ref false,e)) !environment.definitions;
     constants = !environment.constants
   } 

let add_cst name arity fix priority  =
  if fix <> Prefix && (arity < 1 or arity > 2) then 
    failwith "Illegal arity for constant";
  let nsymbol = {
    symbol_name = name;
    symbol_fix = fix;
    symbol_priority = priority;
    symbol_arity = arity;
    symbol_rewrite = []
  } in
  environment := {
     definitions = !environment.definitions;
     constants = StringMap.add name nsymbol !environment.constants
   } 
  
let add_red (arities, pat, res as pattern) =
  let vars = 
    Array.map 
      (fun n -> start_term (bind (fun tl -> unit Dummy)))
      arities
  in 
  let pat = subst pat vars in
  match pat with
    App(_,_,symbol, _) ->
      symbol.symbol_rewrite <- pattern::symbol.symbol_rewrite
  | _ -> failwith "Illegal pattern" 


let do_prn e =
  match e with
    Def(name,_,t) ->
      open_hovbox 2;
      print_string name;
      print_string " =";
      print_space ();
      print_term t;
      close_box ();
      print_newline ();
  | _ ->
      print_term e;
      print_newline ()

let do_show_all () =
  StringMap.iter (fun _ e -> do_prn e) !environment.definitions;
  StringMap.iter (fun _ sym -> print_constants sym) !environment.constants
    
let do_show name =
  let sym = 
    try 
      StringMap.find name !environment.constants
    with Not_found -> raise (Unbound name)
  in
  print_constants sym



