open Basic
open Format

module F_definition =
  struct
    type sort = Star | Box
    exception Ill_rule of sort * sort
    exception Ill_axiom of sort
    let get_axiom  = function
	Star -> Box
      |	s -> raise (Ill_axiom s)
    let get_rule s1 s2 = match s1, s2 with
      Star, Star -> Star
    | Box, Star -> Star
    | s1, s2 -> raise (Ill_rule(s1,s2))
    let print_sort = function
      	Star -> print_string "*"
      |	Box -> print_string "#"
    let keywords = [ "*"; "#"]
    let parser parse_sort =
	'*' -> Star
      |	'#' -> Box
  end

module F = Main.Make(F_definition)


let _ = F.main ()
