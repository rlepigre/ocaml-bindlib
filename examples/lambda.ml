open Bindlib

(* AST of the pure λ-calculus using free variables and binders. *)
type term =
  | Var of term var
  | Abs of (term, term) binder
  | App of term * term

(* The often required [mkfree] function. *)
let mkfree : term var -> term =
  fun x -> Var(x)

(* Smart constructors to build terms in the [bindbox]. *)
let var : string -> term bindbox =
  fun x -> box_of_var (new_var mkfree x)

let abs : string -> (term bindbox -> term bindbox) -> term bindbox =
  fun x f -> box_apply (fun b -> Abs(b)) (bind mkfree x f)

let app : term bindbox -> term bindbox -> term bindbox =
  fun t u -> box_apply2 (fun t u -> App(t,u)) t u

(* Alternative smart constructor for [Abs], using [vbind]. *)
let vabs : string -> (term var -> term bindbox) -> term bindbox =
  fun x f -> box_apply (fun b -> Abs(b)) (vbind mkfree x f)

(* Some examples of terms (not unboxed). *)
let x     : term bindbox = var "x"
let y     : term bindbox = var "y"
let id    : term bindbox = abs "x" (fun x -> x)
let fst   : term bindbox = abs "x" (fun x -> abs "y" (fun y -> x))
let delta : term bindbox = abs "x" (fun x -> app x x)
let swap  : term bindbox = abs "x" (fun x -> abs "y" (fun y -> app y x))
let omega : term bindbox = app delta delta
let fsty  : term bindbox = app fst y
let fstyx : term bindbox = app fsty x
let swapy : term bindbox = app swap y

(* Unboxed versions. *)
let x     : term = unbox x
let y     : term = unbox y
let id    : term = unbox id
let fst   : term = unbox fst
let delta : term = unbox delta
let swap  : term = unbox swap
let omega : term = unbox omega
let fsty  : term = unbox fsty
let fstyx : term = unbox fstyx
let swapy : term = unbox swapy

(* Translation to string. *)
let rec to_string : term -> string = fun t ->
  match t with
  | Var(x)   -> name_of x
  | Abs(b)   -> let (x,t) = unbind mkfree b in
                "λ" ^ name_of x ^ "." ^ to_string t
  | App(t,u) -> "(" ^ to_string t ^ ") " ^ to_string u

(* Printing function. *)
let rec print : out_channel -> term -> unit = fun ch t ->
  match t with
  | Var(x)   -> Printf.fprintf ch "%s" (name_of x)
  | Abs(b)   -> let (x,t) = unbind mkfree b in
                Printf.fprintf ch "λ%s.%a" (name_of x) print t
  | App(t,u) -> Printf.fprintf ch "(%a) %a" print t print u

(* Lifting to the [bindboc]. *)
let rec lift : term -> term bindbox = fun t ->
  match t with
  | Var(x)   -> box_of_var x
  | Abs(b)   -> vabs (binder_name b) (fun x -> lift (subst b (Var(x))))
  | App(u,v) -> app (lift u) (lift v)

(* Update function to recompute names (required after substitution). *)
let update : term -> term = fun t ->
  unbox (lift t)

(* One step evaluation function (call-by-name). *)
let rec step : term -> term option = fun t ->
  match t with
  | App(Abs(b), u) -> Some (subst b u)
  | App(t     , u) ->
      begin
        match step t with
        | None   -> None
        | Some t -> Some (App(t,u))
      end
  | _              -> None

(* Full evaluation (call-by-name). *)
let rec eval : term -> term = fun t ->
  match step t with
  | None   -> t
  | Some t -> eval t

(* Tests. *)
let _ =
  Printf.printf "Here are some terms:\n%!";
  Printf.printf "  %a\n%!" print x;
  Printf.printf "  %a\n%!" print y;
  Printf.printf "  %a\n%!" print id;
  Printf.printf "  %a\n%!" print fst;
  Printf.printf "  %a\n%!" print delta;
  Printf.printf "  %a\n%!" print omega;
  Printf.printf "  %a\n%!" print fsty;
  Printf.printf "  %a\n%!" print fstyx;
  Printf.printf "  %a\n%!" print swapy

let _ =
  Printf.printf "Substitution is fast, but does not handle renaming:\n%!";
  Printf.printf "  %a\n  → %a\n%!" print fsty  print (eval fsty);
  Printf.printf "  %a\n  → %a\n%!" print fstyx print (eval fstyx);
  Printf.printf "  %a\n  → %a\n%!" print swapy print (eval swapy)

let _ =
  Printf.printf "For printing, it is better to update names first:\n%!";
  Printf.printf "  %a\n  → %a\n%!" print fsty  print (update (eval fsty));
  Printf.printf "  %a\n  → %a\n%!" print fstyx print (update (eval fstyx));
  Printf.printf "  %a\n  → %a\n%!" print swapy print (update (eval swapy))

(* Example of parsing time AST for our language. *)
type pterm =
  | PVar of string
  | PLam of string * pterm
  | PApp of pterm * pterm

(* Translation function to our AST using bindlib. *)
let trans : pterm -> term =
  let rec trans : (string * term bindbox) list -> pterm -> term bindbox =
    fun env t ->
      match t with
      | PVar(x)   -> List.assoc x env (* unbound variable if not found *)
      | PLam(x,t) -> abs x (fun v -> trans ((x,v)::env) t)
      | PApp(t,u) -> app (trans env t) (trans env u)
  in
  fun t -> unbox (trans [] t)

(* Alternative translation function. *)
let trans' : pterm -> term =
  let rec trans : (string * term var) list -> pterm -> term bindbox =
    fun env t ->
      match t with
      | PVar(x)   -> box_of_var (List.assoc x env)
      | PLam(x,t) -> vabs x (fun v -> trans ((x,v)::env) t)
      | PApp(t,u) -> app (trans env t) (trans env u)
  in
  fun t -> unbox (trans [] t)

(* Translation test. *)
let _ =
  let fst = PLam("x",PLam("y",PVar("x"))) in
  Printf.printf "Translated to %a and %a\n%!"
    print (trans fst) print (trans' fst)
