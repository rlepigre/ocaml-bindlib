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
	| Proj1(t) ->
      	    fn app_lvl t;
      	    print_string ".1"
	| Proj2(t) ->
      	    fn app_lvl t;
      	    print_string ".2"
	| Pair(t1,t2) ->
	    print_string "[";
	    fn ini_lvl t1;
	    print_string ", ";
	    let rec gn = function
	    	Pair(t2,t3) -> fn ini_lvl t2; print_string ", "; gn t3
	      | t2 ->  fn ini_lvl t2; print_string "]"
	    in gn t2
    	| Lambda(t,f) ->
    	    if b >= abs_lvl then print_string "(";
    	    print_string "λ";
	    let x = new_var fVar (binder_name f) in
	    let u = subst f (free_of x) in
	    let name = name_of x in
	    print_string name;
            let rec gn = function
	      | Lambda(t',f) when equal t t' ->
		 let x = new_var fVar (binder_name f) in
		 let u = subst f (free_of x) in
		 let name = name_of x in
		 print_string ",";
		 print_string name;
		 gn u
              | e ->
		 print_string ":";
		 fn ini_lvl t;
		 print_string ". ";
		 fn (abs_lvl - 1) e
	    in gn u;
    	    if b >= abs_lvl then print_string ")"

        | Pi(t,f) when binder_constant f ->
    	    if b >= arr_lvl then print_string "(";
	    fn arr_lvl t;
	    print_string " → ";
	    let x = new_var fVar (binder_name f) in
	    let u = subst f (free_of x) in
	    fn (if b >= arr_lvl then ini_lvl else b) u;
    	    if b >= arr_lvl then print_string ")"

        | Sigma(t,f) when binder_constant f ->
    	    if b >= arr_lvl then print_string "(";
	    fn arr_lvl t;
	    print_string " × ";
	    let x = new_var fVar (binder_name f) in
	    let u = subst f (free_of x) in
	    fn (if b >= arr_lvl then ini_lvl else b) u;
    	    if b >= arr_lvl then print_string ")"

    	| Pi(t,f) ->
    	    if b >= abs_lvl then print_string "(";
    	    print_string "Π";
	    let x = new_var fVar (binder_name f) in
	    let u = subst f (free_of x) in
	    let name = name_of x in
	    print_string name;
            let rec gn = function
	      | Pi(t',f) when equal t t' ->
		 let x = new_var fVar (binder_name f) in
		 let u = subst f (free_of x) in
		 let name = name_of x in
		 print_string ",";
		 print_string name;
		 gn u
              | e ->
		 print_string ":";
		 fn ini_lvl t;
		 print_string ".";
		 fn (abs_lvl - 1) e
	    in gn u;
    	    if b >= abs_lvl then print_string ")"

    	| Sigma(t,f) ->
    	    if b >= abs_lvl then print_string "(";
    	    print_string "Σ";
	    let x = new_var fVar (binder_name f) in
	    let u = subst f (free_of x) in
	    let name = name_of x in
	    print_string name;
            let rec gn = function
	      | Sigma(t',f) when equal t t' ->
		 let x = new_var fVar (binder_name f) in
		 let u = subst f (free_of x) in
		 let name = name_of x in
		 print_string ",";
		 print_string name;
		 gn u
                | e ->
		   print_string ":";
		   fn ini_lvl t;
		   print_string ".";
		   fn (abs_lvl - 1) e
	      in gn u;
    	      if b >= abs_lvl then print_string ")"

	| Atom(s) ->
	    print_sort s

	| Goal(s) ->
	    print_string "{\"";
	    print_string s;
	    print_string "\"}"

	| Def d ->
    	      print_string d.def_name

    	| FVar v -> print_string (name_of v)

      in fn ini_lvl t

  end
