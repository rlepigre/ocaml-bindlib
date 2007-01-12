open Basic
open Format

module CoC_definition =
  struct
    type sort = Star | Box
    exception Ill_rule of sort * sort
    exception Ill_axiom of sort
    let get_axiom  = function
	Star -> Box
      |	s -> raise (Ill_axiom s)
    let get_rule s1 s2 = s2
    let print_sort = function
      	Star -> print_string "*"
      |	Box -> print_string "#"
    let keywords = [ "*"; "#"]
    let parse_sort = parser 
	[< 'Genlex.Kwd "*" >] -> Star
      |	[< 'Genlex.Kwd "#" >] -> Box
  end

module CoC = Main.Make(CoC_definition)

let _ = CoC.main ()

