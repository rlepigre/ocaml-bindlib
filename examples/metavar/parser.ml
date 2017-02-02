open Earley
open Term
open Command

let parser lident = id:{#[a-z0-9][_a-zA-Z0-9]*[']*#}[group.(0)]

let parser atom =
  | id:lident -> PVar id
  | '(' t:term ')' -> t
  | '[' t:term ']' -> PNrm t

and aterm =
  | atom
  | t:aterm a:atom -> PApp(t,a)

and term =
  | aterm
  | ("λ"|"%") ids:lident* '.' t:term  ->
     List.fold_right (fun id acc -> PLam(id,acc)) ids t

let parser command =
  | "$u"                      -> Undo
  | "$s" id:lident "=" t:term -> Decl(id, t)
  | "$p" t:term               -> Prnt(t)

let parser commands = command*

let _ =
  parse_buffer commands subml_blank (buffer_from_channel stdin)
