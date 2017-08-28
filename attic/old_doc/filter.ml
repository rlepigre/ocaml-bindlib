open Str

let _ =
  if Array.length Sys.argv <> 2 then
    begin
      Printf.eprintf "%s: bad arguments\nusage: filter file\n%!" Sys.argv.(0);
      exit 1
    end
let filename = Sys.argv.(1)
let in_file =
  try open_in filename with _ ->
      Printf.eprintf "%s: can not open %S\n%!" Sys.argv.(0) filename;
      exit 1

let prefix = "\\begin{lstlisting}%"
let len_prefix = String.length prefix

let suffix = "\\end{lstlisting}"

let out_files = Hashtbl.create 7
let open_file name =
  try
    Hashtbl.find out_files name
  with Not_found ->
    let ch = open_out name in
    Hashtbl.add out_files name ch;
    ch

let rec get_filename pos str =
  if pos >= String.length str || List.mem str.[pos] [' ';'\n';'\r';'\t'] then
    []
  else String.make 1 str.[pos] :: get_filename (pos+1) str

let get_filename pos str = String.concat "" (get_filename pos str)

let begin_with prefix =
  let len = String.length prefix in
  (fun line ->
    String.length line >= len && String.sub line 0 len = prefix)

let is_begin = begin_with prefix
let is_end   = begin_with suffix

let _ =
  let in_block = ref None in
  let line_num = ref 0 in
  try
    while true do
      let line = input_line in_file in
      incr line_num;
     match !in_block with
     | None ->
        if is_begin line then
          begin
            let out_filename = get_filename len_prefix line in
            if filename <> "" then
              begin
                let ch = open_file out_filename in
                in_block := Some(!line_num,ch);
                Printf.fprintf ch "# %d %S\n%!" (!line_num+1) filename
              end
          end

     | Some(l,ch) ->
        if is_end line then begin
            in_block := None;
            Printf.fprintf ch "\n"
        end else begin
            Printf.fprintf ch "%s\n" line
        end

    done
  with End_of_file ->
       Hashtbl.iter (fun _ ch -> close_out ch) out_files;
       begin
         match !in_block with
         | None -> ()
         | Some(l, _) ->
            Printf.eprintf "%s: unclosed block at line %d\n%!" Sys.argv.(0) l;
            exit 1
       end;
