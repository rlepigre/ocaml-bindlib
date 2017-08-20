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
let abs : string -> (term bindbox -> term bindbox) -> term bindbox =
  fun x f -> box_apply (fun b -> Abs(b)) (bind mkfree x f)

let app : term bindbox -> term bindbox -> term bindbox =
  fun t u -> box_apply2 (fun t u -> App(t,u)) t u

(* Alternative smart constructor for [Abs], using [bind_var]. *)
let vabs : term var -> term bindbox -> term bindbox =
  fun v t -> box_apply (fun b -> Abs(b)) (bind_var v t)

(* A way to create a variable. Beware that using twice
   [var "x"] creates two distinct variables *)
let var : string -> term var = fun x -> new_var mkfree x

(* A version to create a variable in a context, see below *)
let var_in : ctxt -> string -> term var * ctxt =
  fun ctxt x -> new_var_in ctxt mkfree x

(* A short cut for box_of_var, to use a variable *)
let use : term var -> term bindbox = box_of_var

(* Some examples of terms (not unboxed). *)
let id    : term bindbox = abs "x" (fun x -> x)
let fst   : term bindbox = abs "x" (fun x -> abs "y" (fun y -> x))
let delta : term bindbox = abs "x" (fun x -> app x x)
let swap  : term bindbox = abs "x" (fun x -> abs "y" (fun y -> app y x))
let omega : term bindbox = app delta delta

(* An examples using var and vabs *)
let x     : term var     = var "x"
let y     : term var     = var "y"
let delt2 : term bindbox = vabs x (app (use x) (use x))
let fsty  : term bindbox = app fst (use y)
let fstyx : term bindbox = app fsty (use x)
let swapy : term bindbox = app swap (use y)

(* Unboxed versions. *)
let x     : term = free_of x (* same as [unbox (use x)] *)
let y     : term = free_of y
let id    : term = unbox id
let fst   : term = unbox fst
let delta : term = unbox delta
let delt2 : term = unbox delt2
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

(* Printing function with context, handle renaming. *)
let rec rprint : ctxt -> out_channel -> term -> unit = fun ctxt ch t ->
  match t with
  | Var(x)   -> Printf.fprintf ch "%s" (name_of x)
  | Abs(b)   -> let (x,t,ctxt) = unbind_in ctxt mkfree b in
                Printf.fprintf ch "λ%s.%a" (name_of x) (rprint ctxt) t
  | App(t,u) -> Printf.fprintf ch "(%a) %a" (rprint ctxt) t (rprint ctxt) u

(* Lifting to the [bindboc]. *)
let rec lift : term -> term bindbox = fun t ->
  match t with
  | Var(x)   -> box_of_var x
  | Abs(b)   -> let (x,t) = unbind mkfree b in
                vabs x (lift (subst b (Var(x))))
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
  Printf.printf "For printing, we can update names first:\n%!";
  Printf.printf "  %a\n  → %a\n%!" print fsty  print (update (eval fsty));
  Printf.printf "  %a\n  → %a\n%!" print fstyx print (update (eval fstyx));
  Printf.printf "  %a\n  → %a\n%!" print swapy print (update (eval swapy))

(* to use context we recreate [x] and [y] in a context *)
let ctxt     : ctxt            = empty_ctxt
let (_,ctxt) : term var * ctxt = var_in ctxt "x"
let (_,ctxt) : term var * ctxt = var_in ctxt "y"

let _ =
  Printf.printf "or use context in printing";
  Printf.printf "  %a\n  → %a\n%!" print fsty  (rprint ctxt) (eval fsty);
  Printf.printf "  %a\n  → %a\n%!" print fstyx (rprint ctxt) (eval fstyx);
  Printf.printf "  %a\n  → %a\n%!" print swapy (rprint ctxt) (eval swapy)

let _ =
  Printf.printf "remark: with context we ensure DeBruijn convention\n%!";
  Printf.printf "  %a versus %a\n%!" print id (rprint ctxt) id

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
      | PLam(x,t) -> let v = var x in
                     vabs v (trans ((x,v)::env) t)
      | PApp(t,u) -> app (trans env t) (trans env u)
  in
  fun t -> unbox (trans [] t)

(* Translation test. *)
let _ =
  let fst = PLam("x",PLam("y",PVar("x"))) in
  Printf.printf "Translated to %a and %a\n%!"
    print (trans fst) print (trans' fst)
