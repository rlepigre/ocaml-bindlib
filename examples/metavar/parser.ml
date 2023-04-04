open Pacomb
open Ast

let%parser ident = (id::RE"[a-z0-9][_a-zA-Z0-9]*[']*") => id

let%parser rec term_atom =
    (id::ident)            => PVar id
  ; '(' (t::term_full) ')' => t
  ; '[' (t::term_full) ']' => PNrm t

and term_appl =
    (t::term_atom) => t
  ; (t::term_appl) (u::term_atom) => PApp(t,u)

and term_full =
    (t::term_appl) => t
  ; ("λ" => () ; "%" => ()) (ids:: ~* ident) '.' (t::term_full)
      => many_PLam ids t

let%parser command =
    "$u"                               => Undo
  ; "$g"                               => Goal
  ; "$s" (x::ident) "=" (t::term_full) => Decl(x,t)
  ; "$p" (t::term_full)                => Prnt(t)

let%parser commands = cs :: ~* ((c::command) ";" => c) => cs

let blank = Blank.line_comments "//"

let parse_channel : in_channel -> cmd list = fun ic ->
  let parse = Grammar.parse_channel commands blank in
  Pos.handle_exception parse ic
