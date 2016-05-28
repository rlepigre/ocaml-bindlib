open Basic
open Format

type variable = int


module CoC_definition =
  struct
    type sort = Star | Box of variable
    exception Ill_rule of sort * sort
    exception Ill_axiom of sort

    let get_axiom  = function
	Star -> Box 0
      | Box v -> Box (v+1)

    let get_rule s1 s2 = s2

    let print_sort = function
      	Star -> print_string "*"
      |	Box 0 -> print_string "#"
      |	Box n -> print_string "#"; print_int n

    let keywords = [ "*"; "#"]

    let parser parse_sort =
	'*' -> Star
      |	'#' -> Box 0
      | '#' s:''[0-9]+'' -> Box (int_of_string s)
  end

module CoC = Main.Make(CoC_definition)

let _ = CoC.main ()
