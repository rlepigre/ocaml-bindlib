open Earley_core
open Ast

let parser ident = id:{#[a-z0-9][_a-zA-Z0-9]*[']*#}[group.(0)]

let parser term_atom =
  | id:ident            -> PVar id
  | '(' t:term_full ')' -> t
  | '[' t:term_full ']' -> PNrm t

and parser term_appl =
  | term_atom
  | t:term_appl u:term_atom -> PApp(t,u)

and parser term_full =
  | term_appl
  | {"λ"|"%"} ids:ident* '.' t:term_full -> many_PLam ids t

let parser command =
  | "$u"                         -> Undo
  | "$g"                         -> Goal
  | "$s" x:ident "=" t:term_full -> Decl(x,t)
  | "$p" t:term_full             -> Prnt(t)

let parser commands = {command ";"}*

let parse_channel : in_channel -> cmd list = fun ic ->
  let parse = Earley.parse_buffer commands Blanks.ocaml_blank in
  Earley.handle_exception parse (Input.from_channel ic)
