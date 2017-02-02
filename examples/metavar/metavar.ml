open Earley
open Term
open Command
open Bindlib

(* a micro parser with decap *)
(* TODO put position in the AST and recover from errors *)
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
  | {"λ"|"%"} ids:lident* '.' t:term  ->
     List.fold_right (fun id acc -> PLam(id,acc)) ids t

let parser command =
  | "$u"                      -> Undo
  | "$g"                      -> Goal
  | "$s" id:lident "=" t:term -> Decl(id, t)
  | "$p" t:term               -> Prnt(t)

let env = { glob = ref []; ctxt = ref empty_ctxt;
            locl = []; undo = [] }

let parser main =
  | EMPTY
  | _:main {c:command -> run env c} ';'

let _ =
  handle_exception (parse_buffer main Blank.blank)
                   (Input.from_channel stdin)
