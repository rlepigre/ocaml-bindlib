open Term
open Bindlib
open Timed
open Ast

type env =
  { glob : meta list ref (* assoc list of all meta var *)
  ; ctxt : ctxt ref      (* bindlib context to use bindlib renaming *)
  ; locl : (string * term box) list
                         (* local environment *)
  ; mutable undo : Time.t list
                         (* do not manage undo list via Timed *)
  }

(* printing of meta var *)
let print_mta ch m =
  let parray ch a =
    Array.iteri (fun i s ->
        Printf.fprintf ch "%s%s" (if i > 0 then "," else "") s) a
  in
  Printf.fprintf ch "%s[%a]" m.mname parray m.ctxte

(* creation of a fresh meta variable. Its context is
   computed from the local environment *)
let create_mta name env =
    let names, args = List.split env.locl in
    let names = Array.of_list names in
    (* let bindlib rename the metavar if needed *)
    let v, ctxt = new_var_in !(env.ctxt) var name in
    let name' = name_of v in
    if name' <> name then
      Printf.printf "Warning: meta variable %s created as %s.\n%!" name name';
    let m = { mname = name'; ctxte = names; value = ref None } in
    Printf.printf "New meta %a\n%!" print_mta m;
    (* update the environment *)
    env.ctxt := ctxt;
    env.glob := m :: ! (env. glob);
    (m, Array.of_list args)

(* search a name in the environment *)
let search name env =
  try (* a local variable ? *)
    List.assoc name env.locl
  with Not_found -> try (* a meta variable with an empty context *)
    let m = List.find (fun m -> m.mname = name) !(env.glob) in
    if m.ctxte <> [||] then raise Not_found;
    mta m [||]
  with Not_found -> (* creation of a fresh meta variable *)
    let m, args = create_mta name env in
    mta m args

(* translation from parsed term to real term *)
let pterm_to_term env t =
  let rec fn env = function
    | PApp(t,u) -> app (fn env t) (fn env u)
    | PLam(name, t) ->
       let v = new_var var name in
       let env = { env with locl = (name, box_var v) :: env.locl } in
       lam v (fn env t)
    | PVar name -> search name env
    | PNrm t    -> norm (unbox (fn env t)) (* local normalisation of a subterm
                                              useless but fun ? *)
  in fn env t

(* undo provided by Timed *)
let undo env = match env.undo with
  | [] -> Printf.printf "Warning: nothing to undo.\n%!"
  | time::undo ->
     Time.restore time; env.undo <- undo

let declare env name t =
  let mta = (* search the mta *)
    try  List.find (fun m -> m.mname = name) !(env.glob)
    with Not_found -> (* or create a new one with an empty context *)
      let m, args = create_mta name env in
      assert (args = [||]); (* check for emptyness of the context *)
      m
  in
  if !(mta.value) <> None then (* can not reset a meta ... use undo *)
    Printf.printf "Warning: meta variable %s already set.\n%!" name
  else
    let vars = new_mvar var mta.ctxte in
    let env = (* prepare the initial local env with the context of the meta *)
      { env with
        locl = Array.to_list
                 (Array.mapi (fun i name -> (name, box_var vars.(i)))
                             mta.ctxte)
               @ env.locl
      }
    in
    (* conversion *)
    let t = pterm_to_term env t in
    (* binding the context and unboxing *)
    let f = unbox (bind_mvar vars t) in
    Printf.printf "Set meta %a\n%!" print_mta mta;
    mta.value := Some f

(* run command *)
let run env = function
  | Decl(name,t) ->
     let save = Time.save () in
     declare env name t;
     env.undo <- save::env.undo
  | Undo ->
     undo env
  | Prnt t ->
     let save = Time.save () in
     Printf.printf "%a\n%!" (print_term !(env.ctxt))
                   (unbox (pterm_to_term env (PNrm t)));
     Time.restore save (* do not keep meta var created just for printing *)
  | Goal ->
     List.iter (fun m -> if !(m.value) = None then
                               Printf.printf "Goal: %a\n%!" print_mta m)
               !(env.glob)

let run =
  let env =
    { glob = Timed.ref []
    ; ctxt = Timed.ref Bindlib.empty_ctxt
    ; locl = []
    ; undo = [] }
  in
  fun cmd -> run env cmd


