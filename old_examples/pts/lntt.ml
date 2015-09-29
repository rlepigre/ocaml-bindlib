open Basic
open Format

module LNTT_definition =
  struct
    type sort = Star_t | Box_t | Star_p | Box_p
    exception Ill_rule of sort * sort
    exception Ill_axiom of sort
    let get_axiom  = function
	Star_t -> Box_t
      | Star_p -> Box_p
      |	s -> raise (Ill_axiom s)
    let get_rule s1 s2 = match s1, s2 with
        Star_t, Star_t -> Star_t
      | Star_p, Star_p -> Star_p
      | Star_t, Box_t -> Box_t
      | Star_p, Box_p -> Box_p
      | Star_t, Box_p -> Star_t
      |	s,s' -> raise (Ill_rule(s,s'))
    let print_sort = function
      	Star_t -> print_string "*"
      |	Box_t -> print_string "#"
      |	Star_p -> print_string "**"
      |	Box_p -> print_string "##"
    let keywords = [ "*"; "#"; "**"; "##"]
    let parse_sort = parser 
	[< 'Genlex.Kwd "*" >] -> Star_t
      |	[< 'Genlex.Kwd "#" >] -> Box_t
      | [< 'Genlex.Kwd "**" >] -> Star_p
      |	[< 'Genlex.Kwd "##" >] -> Box_p
  end

module LNTT = Main.Make(LNTT_definition)

let _ = LNTT.main ()

