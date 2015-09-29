open Basic
open Bindlib
open Format

module Make(Pts: PtsType) =
  struct
    open Pts
    module GlobalsPts = Globals.Make(Pts)
    open GlobalsPts
    open BasicPts


    let app_lvl = 3
    let abs_lvl = 2
    let arr_lvl = 1
    let ini_lvl = 0

    let print_expr t =
      let t = unbox (lift_expr t) in
      let rec fn b = function
	  App(t1,t2) ->
      	    if b >= app_lvl then print_string "(";
      	    fn (app_lvl - 1) t1;
      	    print_string " ";
      	    fn app_lvl t2;
      	    if b >= app_lvl then print_string ")"

    	| Lambda(t,f) ->
    	    if b >= abs_lvl then print_string "(";
    	    print_string "\\";
	    begin match f with bind fVar x in u ->
	      let name = name_of x in
	      print_string name;
              let rec gn = function
		  Lambda(t',f) when equal t t' ->
		    begin match f with bind fVar x in u ->
		      let name = name_of x in
		      print_string ",";
		      print_string name;
		      gn u
		    end
                | e ->
		    print_string ":";
		    fn ini_lvl t;
		    print_string ". ";
		    fn (abs_lvl - 1) e
	      in gn u;
    	      if b >= abs_lvl then print_string ")"
	    end
        | Pi(t,f) when is_binder_closed f ->
    	    if b >= arr_lvl then print_string "(";
	    fn arr_lvl t;
	    print_string " -> ";
	    begin match f with bind fVar x in u ->
	      fn (if b >= arr_lvl then ini_lvl else b) u
	    end;
    	    if b >= arr_lvl then print_string ")"

    	| Pi(t,f) ->
    	    if b >= abs_lvl then print_string "(";
    	    print_string "/\\";
	    begin match f with bind fVar x in u ->
	      let name = name_of x in
	      print_string name;
              let rec gn = function
		  Pi(t',f) when equal t t' ->
		    begin match f with bind fVar x in u ->
		      let name = name_of x in
		      print_string ",";
		      print_string name;
		      gn u
		    end
                | e ->
		    print_string ":";
		    fn ini_lvl t;
		    print_string ".";
		    fn (abs_lvl - 1) e
	      in gn u;
    	      if b >= abs_lvl then print_string ")"
	    end

	| Atom(s) ->
	    print_sort s

	| Def d ->
	    if d.def_islocal then begin
	      let name = "!"^d.def_name^(string_of_int d.def_generation) in
	      print_string "(";
    	      print_string name;
	      (match d.def_value with 
	        Some e -> 
		  print_string "=";
		  fn ini_lvl e
              | _ ->
		  ());
	      print_string ":";
	      fn ini_lvl d.def_type;
	      print_string ")"
	    end else
    	      print_string d.def_name

    	| FVar v -> print_string (name_of v)

      in fn ini_lvl t

  end


