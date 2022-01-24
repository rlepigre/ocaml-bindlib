(** Representation of pure λ-terms. *)
type term =
  | Var of term Bindlib.var
  (** Free variables. *)
  | Abs of (term, term) Bindlib.binder
  (** Abstraction. *)
  | App of term * term
  (** Application. *)

(** Synonym for the type of λ-variables. *)
type tvar = term Bindlib.var

(** Synonym for the type of boxed terms. *)
type tbox = term Bindlib.box

(** Injection of free variables into terms. *)
let mkfree : tvar -> term = fun x -> Var(x)

(** Smart constructor for free variables. *)
let var : tvar -> tbox = Bindlib.box_var

(** Smart constructor for abstractions. *)
let abs : (term, term) Bindlib.binder Bindlib.box -> tbox =
  Bindlib.box_apply (fun b -> Abs(b))

(** Smart constructor for applications. *)
let app : tbox -> tbox -> tbox =
  Bindlib.box_apply2 (fun t u -> App(t,u))

(** Printing function for terms. *)
let print_term : out_channel -> term -> unit =
  let open Printf in
  let rec print (p : [`Atm | `App | `Fun]) oc t =
    match (t, p) with
    | (Var(x)  , _   ) -> output_string oc (Bindlib.name_of x)
    | (Abs(b)  , `Fun) -> let (x, t) = Bindlib.unbind b in
                          fprintf oc "λ%s.%a" (Bindlib.name_of x) (print p) t
    | (App(t,u), `Fun)
    | (App(t,u), `App) -> fprintf oc "%a %a" (print `App) t (print `Atm) u
    | (_       , _   ) -> fprintf oc "(%a)" (print `Fun) t
  in
  print `Fun

(** Weak head normalization function. *)
let wh_norm : term -> term = fun t ->
  let rec wh_norm t s =
    match (t, s) with
    | (App(t,u), _   ) -> wh_norm t (u::s)
    | (Abs(f)  , a::s) -> wh_norm (Bindlib.subst f a) s
    | (_       , _   ) -> List.fold_left (fun t u -> App(t,u)) t s
  in
  wh_norm t []

(** Full normalization. *)
let norm : term -> term = fun t ->
  let rec norm : term -> term list -> tbox = fun t s ->
    match t with
    | Var(x)   -> List.fold_left (fun t u -> app t (norm u [])) (var x) s
    | App(t,u) -> norm t (u::s)
    | Abs(f)   ->
        match s with
        | a::s -> norm (Bindlib.subst f a) s
        | []   -> let (x,t) = Bindlib.unbind f in
                  abs (Bindlib.bind_var x (norm t []))
  in
  Bindlib.unbox (norm t [])

(** Examples of terms. *)

let (>>=) x t = abs (Bindlib.bind_var x t)
let (!!) x = var x

let x = Bindlib.new_var mkfree "x"
let y = Bindlib.new_var mkfree "y"
let t = Bindlib.new_var mkfree "t"
let f = Bindlib.new_var mkfree "f"
let n = Bindlib.new_var mkfree "n"
let m = Bindlib.new_var mkfree "m"
let p = Bindlib.new_var mkfree "p"

let id    = Bindlib.unbox (x >>= !!x)
let delta = Bindlib.unbox (x >>= app !!x !!x)
let zero  = Bindlib.unbox (f >>= (x >>= !!x))
let tfls  = Bindlib.unbox (t >>= (f >>= !!f))
let ttru  = Bindlib.unbox (t >>= (f >>= !!t))

let succ  =
  Bindlib.unbox (n >>= (f >>= (x >>= (app !!f (app (app !!n !!f) !!x)))))

let succ' =
  Bindlib.unbox (n >>= (f >>= (x >>= (app (app !!n !!f) (app !!f !! x)))))

let plus  = Bindlib.unbox
  (n >>= (m >>= (f >>= (x >>= (app (app !!n !!f) (app (app !!m !!f) !!x))))))

let mult  = Bindlib.unbox
  (n >>= (m >>= (f >>= (app !!n (app !!m !!f)))))

let pred  = Bindlib.unbox
  (n >>= app (app (app (app !!n (p >>= (x >>= (y >>= (
    app (app !!p (app (Bindlib.box succ) !!x)) !!x)))))
	  (x >>= (y >>= !!y))) (Bindlib.box zero)) (Bindlib.box zero))

let ch_2   = App(succ, App(succ, zero))
let ch_4   = App(ch_2, ch_2)
let ch_8   = App(App(plus, ch_4), ch_4)
let ch_10  = App(App(plus, ch_2), ch_8)
let ch_100 = App(App(mult, ch_10), ch_10)
let ch_1000= App(App(mult, ch_100), ch_10)

let bench () =
  let fh = App(App(mult, ch_4), ch_100 ) in
  let ft = App(App(mult, ch_4), ch_1000) in
  let res = norm (App(App(ft, pred), ft)) in
  Printf.printf "Result: %a\n%!" print_term res;
  (*let res = norm (App(App(mult, fh), ch_1000)) in
  Printf.printf "Result: %a\n%!" print_term res;*)
  let _ = norm (App(App(mult, fh), ch_100)) in
  Printf.printf "Result: ...\n%!";
  let _ = norm (App(App(mult, fh), ch_1000)) in
  let zz = norm (App(App(ch_1000,pred),ch_1000)) in
  Printf.printf "Result: %a\n%!" print_term zz;
  Printf.printf "Top heap: %d\n%!" Gc.((stat ()).top_heap_words)

let _ = bench ()
