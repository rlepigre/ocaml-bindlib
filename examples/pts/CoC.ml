open Basic
open Format

module CoC_definition =
  struct
    type sort = Star | Box | Typ1
    exception Ill_rule of sort * sort
    exception Ill_axiom of sort

    let get_axiom  = function
	Star -> Box
      | Box -> Typ1
      |	s -> raise (Ill_axiom s)

    let get_rule s1 s2 = s2

    let print_sort = function
      	Star -> print_string "*"
      |	Box -> print_string "#"
      |	Typ1 -> print_string "@"

    let keywords = [ "*"; "#"]

    let parser parse_sort =
	'*' -> Star
      |	'#' -> Box
      |	'@' -> Typ1
  end

module CoC = Main.Make(CoC_definition)

let _ = CoC.main ()
