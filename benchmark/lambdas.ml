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
  let rec print ctx (p : [`Atm | `App | `Fun]) oc t =
    match (t, p) with
    | (Var(x)  , _   ) -> output_string oc (Bindlib.name_of x)
    | (Abs(b)  , `Fun) -> let (x, t,ctx) = Bindlib.unbind_in ctx b in
                          fprintf oc "λ%s.%a" (Bindlib.name_of x)
                                              (print ctx p) t
    | (App(t,u), `Fun)
      | (App(t,u), `App) -> fprintf oc "%a %a" (print ctx `App) t
                                               (print ctx `Atm) u
    | (_       , _   ) -> fprintf oc "(%a)" (print ctx `Fun) t
  in
  print Bindlib.empty_ctxt `Fun

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

let top0 = Gc.(stat ()).minor_words

(** Examples of terms. *)
let (>>=) x t = abs (Bindlib.bind_var x t)
let (!!) x = var x

let x = Bindlib.new_var mkfree "x"
let y = Bindlib.new_var mkfree "y"
let z = Bindlib.new_var mkfree "z"
let t = Bindlib.new_var mkfree "t"
let f = Bindlib.new_var mkfree "f"
let l = Bindlib.new_var mkfree "l"
let n = Bindlib.new_var mkfree "n"
let m = Bindlib.new_var mkfree "m"
let p = Bindlib.new_var mkfree "p"
let q = Bindlib.new_var mkfree "q"
let r = Bindlib.new_var mkfree "r"
let app2 f x y = app(app f x) y
let bb f = Bindlib.box f
let bb1 f x = app (Bindlib.box f) x
let bb2 f x y = app2 (Bindlib.box f) x y

let id    = Bindlib.unbox (x >>= !!x)
let delta = Bindlib.unbox (x >>= app !!x !!x)
let zero  = Bindlib.unbox (f >>= (x >>= !!x))
let tfls  = Bindlib.unbox (t >>= (f >>= !!f))
let ttru  = Bindlib.unbox (t >>= (f >>= !!t))
let delta_fix
          = (x >>= (f >>= app !!f (app2 !!x !!x !!f)))
let fix   = Bindlib.unbox (app delta_fix delta_fix)

let succ  =
  Bindlib.unbox (n >>= (f >>= (x >>= (app !!f !!n))))

let plus  =
  Bindlib.unbox
    (bb1 fix (r >>= (n >>= (m >>= (
         app2 !!n (p >>= bb1 succ (app2 !!r !!m !!p)) !!m)))))

let mult  =
  Bindlib.unbox
    (bb1 fix (r >>= (n >>= (m >>= (
         app2 !!n (p >>= bb2 plus (app2 !!r !!m !!p) !!m) (bb zero))))))

let pred  =
  Bindlib.unbox
    (n >>= (app2 !!n (p >>= !!p) (bb zero)))

let minus =
  Bindlib.unbox
    (bb1 fix (r >>= (n >>= (m >>= (
         app2 !!m (p >>= bb1 pred (app2 !!r !!n !!p)) !!n)))))

let leq   =
  Bindlib.unbox
    (bb1 fix (r >>= (n >>= (m >>= (
         app2 !!n (p >>= app2 !!m (q >>=
           app2 !!r !!p !!q) (bb tfls)) (bb ttru))))))

let le   =
  Bindlib.unbox
    (bb1 fix (r >>= (n >>= (m >>= (
         app2 !!m (q >>= app2 !!n (p >>=
           app2 !!r !!p !!q) (bb ttru)) (bb tfls))))))

let modul =
  Bindlib.unbox
    (bb1 fix (r >>= (n >>= (m >>= (
         app2 (app2 (bb le) !!n !!m) !!n
           (app2 !!r (app2 (bb minus) !!n !!m) !!m))))))

let iter   =
  Bindlib.unbox
    (bb1 fix (r >>= (n >>= (f >>= (x >>= (
         app2 !!n (p >>= app !!f (app2 (app !!r !!p) !!f !!x)) !!x))))))

let nil    = zero
let cons   =
  Bindlib.unbox (y >>= (l >>= (f >>= (x >>= app2 !!f !!y !!l))))

let ltree  =
  Bindlib.unbox
    (app (bb1 fix (r >>= (l >>= (n >>= (
         app2 !!n (p >>= (y >>= (z >>=
           app2 !!y (app2 !!r (app2 (bb cons) !!y !!l) !!p)
           (app2 !!r (app2 (bb cons) !!z !!l) !!p)))) !!l))))) (bb nil))
let sc_1   = App(succ, zero)
let sc_2   = App(succ, sc_1)
let sc_3   = App(succ, sc_2)
let sc_4   = App(App(mult,sc_2), sc_2)
let sc_6   = App(App(mult,sc_4), sc_2)
let sc_8   = App(App(plus, sc_4), sc_4)
let sc_10  = App(App(plus, sc_2), sc_8)
let sc_15  = App(succ,App(App(plus, sc_10), sc_4))
let sc_100 = App(App(mult, sc_10), sc_10)
let sc_1000= App(App(mult, sc_100), sc_10)
let sc_10000= App(App(mult, sc_100), sc_100)
let sc_1000000= App(App(mult, sc_1000), sc_1000)

let bench () =
  let res  = norm (App(pred,sc_4)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(leq,sc_2), sc_3)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(leq,sc_3), sc_3)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(leq,sc_3), sc_2)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(le,sc_2), sc_3)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(le,sc_3), sc_3)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(le,sc_3), sc_2)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(modul,sc_100), sc_3)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(modul,App(succ,sc_100)), sc_3)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let res = norm (App(App(modul,App(succ,App(succ,sc_100))), sc_3)) in
  Printf.printf "Result: %a\n%!" print_term res;
  let _ = norm (App(ltree,sc_15)) in
  Printf.printf "Result: ...\n%!";
  Printf.printf "Minor words: %f\n%!" Gc.((stat ()).minor_words -. top0)

let _ = bench ()
