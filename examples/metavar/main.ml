(** Entry point. *)

let _ =
  try
    let cmds = Parser.parse_channel stdin in
    List.iter Cmd.run cmds
  with e ->
    Printf.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string e);
    exit 1
