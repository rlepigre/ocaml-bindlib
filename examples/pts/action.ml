open Basic
open Bindlib
open Format

module Make(Pts: PtsType) =
  struct
    open Pts
    module Type_checkPts = Type_check.Make(Pts)
    open Type_checkPts
    open PrintPts
    open GlobalsPts
    open BasicPts

    exception Ill_tactic of string
	  
    let define name e t =
      	type_check e t;
	print_string name;
	print_string " = ";
	print_expr e;
	print_string " : ";
	print_expr t;
	print_newline ();
      	let d = {
	  def_name = name;
	  def_value = Some e;
	  def_type = t;
	  def_generation = gen_sym ();
	  def_islocal = false
	} in
	add_def d; Def d

  end


