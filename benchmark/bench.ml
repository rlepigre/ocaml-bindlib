type term =
  | Var of term Bindlib.var
  | Abs of (term, term) Bindlib.binder
  | App of term * term

type tbinder = (term, term) Bindlib.binder
type tvar = term Bindlib.var
type tbox = term Bindlib.box

let mkfree : tvar -> term = fun x -> Var(x)

let var : tvar -> tbox =
  Bindlib.box_var

let app : tbox -> tbox -> tbox =
  Bindlib.box_apply2 (fun t u -> App(t,u))

let abs : tbinder Bindlib.box -> tbox =
  Bindlib.box_apply (fun b -> Abs(b))

let rec box_term : term -> tbox = fun t ->
  match t with
  | Var(x)   -> var x
  | Abs(b)   -> abs (Bindlib.box_binder box_term b)
  | App(t,u) -> app (box_term t) (box_term u)

type 'a pp = out_channel -> 'a -> unit

let pp_term_in : Bindlib.ctxt -> term pp = fun ctxt oc t ->
  let pp fmt = Printf.fprintf oc fmt in
  let paren_if b = if b then ("(", ")") else ("", "") in
  let app_lvl = 2 in
  let abs_lvl = 1 in
  let ini_lvl = 0 in
  let rec print ctxt b _ t =
    match t with
    | Var(x)   ->
        pp "%s" (Bindlib.name_of x)
    | App(t,u) ->
        let (l, r) = paren_if (b >= app_lvl) in
        pp "%s%a %a%s" l (print ctxt abs_lvl) t (print ctxt app_lvl) u r
    | Abs(_)   ->
        let (l, r) = paren_if (b >= abs_lvl) in
        let rec pp_abs ctxt _ t =
          match t with
          | Abs(b) -> let (x,t,ctxt) = Bindlib.unbind_in ctxt b in
                      pp " %s%a" (Bindlib.name_of x) (pp_abs ctxt) t
          | _      -> pp ".%a" (print ctxt ini_lvl) t
        in
        pp "%sλ%a%s" l (pp_abs ctxt) t r
  in
  print ctxt ini_lvl oc t

let pp_term : term pp = fun oc t ->
  let ctxt = Bindlib.free_vars (box_term t) in
  pp_term_in ctxt oc t

let norm : term -> term = fun t ->
  let rec norm t stack =
    match t with
    | App(t,u) -> norm t (u :: stack)
    | Abs(b)   ->
        begin
          match stack with
          | u :: stack -> norm (Bindlib.subst b u) stack
          | []         ->
              if Bindlib.binder_closed b then Bindlib.box t else
              let (x,t) = Bindlib.unbind b in
              abs (Bindlib.bind_var x (norm t []))
        end
    | Var(x)   ->
        List.fold_left (fun t u -> app t (norm u [])) (var x) stack
  in
  Bindlib.unbox (norm t [])

let lam x f =
  let x = Bindlib.new_var mkfree x in
  abs (Bindlib.bind_var x (f (var x)))

let zero =
  lam "f" (fun _ -> lam "x" (fun x -> x))
let zero_ = Bindlib.unbox zero

let succ =
  lam "n" (fun n -> lam "f" (fun f -> lam "x" (fun x ->
    app f (app (app n f) x)
  )))
let succ_ = Bindlib.unbox succ

let plus =
  lam "n" (fun n -> lam "m" (fun m -> lam "f" (fun f -> lam "x" (fun x ->
    app (app n f) (app (app m f) x)
  ))))
let plus_ = Bindlib.unbox plus

let mult =
  lam "n" (fun n -> lam "m" (fun m -> lam "f" (fun f ->
    app n (app m f))))
let mult_ = Bindlib.unbox mult

let pred =
  lam "n" (fun n ->
    app (app (app (app n (
      lam "p" (fun p -> lam "x" (fun x -> lam "y" (fun _ ->
        app (app p (app succ x)) x
      ))))) (lam "x" (fun _ -> lam "y" (fun y -> y)))) zero) zero
  )
let pred_ = Bindlib.unbox pred

let nat_2    = App(succ_, App(succ_, zero_))
let nat_4    = App(nat_2, nat_2)
let nat_8    = App(App(plus_, nat_4), nat_4)
let nat_10   = App(App(plus_, nat_2), nat_8)
let nat_100  = App(App(mult_, nat_10), nat_10)
let nat_400  = App(App(mult_, nat_4), nat_100)
let nat_1000 = App(App(mult_, nat_100), nat_10)
let nat_4000 = App(App(mult_, nat_4), nat_1000)

let t0 = App(App(nat_4000, pred_), nat_4000)
let t1 = App(App(mult_, nat_400), nat_1000)

let _ =
  Printf.printf "Normalising: %a\n%!" pp_term t0;
  let t0_nf = norm t0 in
  Printf.printf "Result: %a\n%!" pp_term t0_nf;
  Printf.printf "Normalising \"%a\"\n%!" pp_term t1;
  let _ = norm t1 in
  Printf.printf "Result: ... (too big)\n%!";
  (* let t1_nf = norm t1 in *)
  (* Printf.printf "%a\n%!" pp_term t1_nf; *)
  let max_words = Gc.((stat ()).top_heap_words) in
  Printf.eprintf "Max major heap size (in words): %i\n%!" max_words
