open Basic
open Bindlib

module Make(Pts: PtsType) =
  struct
    module BasicPts = Basic.Make(Pts)
    open BasicPts

    module StringCmp =
      struct
      	type t = string
    	let compare = compare
      end

    module StringSet = Set.Make(StringCmp)
    module StringMap = Map.Make(StringCmp)

    type environment = {
	env_def : def StringMap.t;
	env_meta : def StringMap.t;
	env_constraints : (expr * expr list * expr * expr list) StringMap.t
      }

    let environment = ref {
      env_def = StringMap.empty;
      env_meta = StringMap.empty;
      env_constraints = StringMap.empty
    }

    let find_name env name =
      try List.assoc name env with Not_found ->
      try box (Def(StringMap.find name !environment.env_def))
      with Not_found -> raise (Unbound name)

    let add_local env name x =
      (name, x)::env

    let add_def d =
      environment :=  {
	 env_def = StringMap.add d.def_name d !environment.env_def;
	 env_meta = !environment.env_meta;
	 env_constraints = !environment.env_constraints
       }

  end
