open Basic
open Bindlib
open Print

type environment = {
  definitions : term StringMap.t;
  constants : symbol StringMap.t;
  } 

let sapp = {
  symbol_name = ""; symbol_arity = 2; symbol_rewrite = [];
  symbol_fix = Infix(Left); symbol_priority = app_lvl;
} 

let environment = ref {
  definitions = StringMap.empty;
  constants = StringMap.add "" sapp StringMap.empty;
} 

let lookup name =
  StringMap.find name !environment.constants

let find_name env name =
  try List.assoc name env with Not_found ->
  try unit (StringMap.find name !environment.definitions) with Not_found ->
  raise (Unbound name)

let find_local env name =
  try List.assoc name env with Not_found ->
  raise (Unbound name)

