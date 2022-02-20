module S = Set.Make(
  struct
    type t = int
    let compare = (-)
  end)

module type Lambda = sig
  type term

  val idx : int -> term
  val abs : term -> term
  val app : term -> term -> term

  val norm : term -> term

  val pp_term : out_channel -> term -> unit
end

module Simple1 : Lambda = struct
  type term =
    | Abs of term
    | Idx of int
    | App of term * term

  type closure =
    | Clos of term * env
    | Var of int

  and env = closure list

  let idx n = Idx(n)
  let abs t = Abs(t)
  let app t u = App(t,u)

  let norm t =
    let rec norm (env:env) (stack:env) depth t =
      match t with
      | Abs(t)         ->
          begin
            match stack with
            | [] ->
                let depth = depth+1 in
                Abs(norm (Var(depth)::env) stack depth t)
            | c :: stack ->
                norm (c::env) stack depth t
          end
      (* Optimisation. *)
      | App(t1,Idx(n)) -> norm env (List.nth env n ::stack) depth t1
      | App(t1,t2)     -> norm env (Clos(t2,env) :: stack) depth t1
      | Idx(n)         ->
          let rec gn stack cl =
            match cl with
            | Clos(t,env) -> norm env stack depth t
            | Var(d)     ->
                let head = Idx(depth - d) in
                List.fold_left (fun t cl -> App(t, gn [] cl)) head stack
          in
          gn stack (List.nth env n)
    in
    norm [] [] 0 t

  let pp_term : out_channel -> term -> unit = fun oc t ->
    let pp fmt = Printf.fprintf oc fmt in
    let paren_if b = if b then ("(", ")") else ("", "") in
    let app_lvl = 2 in
    let abs_lvl = 1 in
    let ini_lvl = 0 in
    let rec print env nv b _ t =
      match t with
      | App(t,u) ->
          let (l, r) = paren_if (b >= app_lvl) in
          pp "%s%a %a%s" l (print env nv abs_lvl) t (print env nv app_lvl) u r
      | Abs(_)   ->
          let (l, r) = paren_if (b >= abs_lvl) in
          let rec pp_abs env nv _ t =
            match t with
            | Abs(t) -> pp " x%i%a" nv (pp_abs (nv :: env) (nv + 1)) t
            | _      -> pp ".%a" (print env nv ini_lvl) t
          in
          pp "%sλ%a%s" l (pp_abs env nv) t r
      | Idx(n)   ->
          pp "x%i" (List.nth env n)
    in
    print [] 0 ini_lvl oc t
end

module Simple2 : Lambda = struct
  type term =
    | Abs of term
    | Idx of int
    | App of term * term
    | Var of int

  let idx n = Idx(n)
  let abs t = Abs(t)
  let app t u = App(t,u)

  let subst v t =
    let rec subst d t0 =
      match t0 with
      | Abs(t)   -> let new_t = subst (d + 1) t in
                    if new_t != t then Abs(new_t) else t0
      | App(t,u) -> let new_t = subst d t in
                    let new_u = subst d u in
                    if new_t != t || new_u != u then App(new_t, new_u) else t0
      | Idx(n)   -> if n = d then v else t0
      | Var(_)   -> t0
    in
    subst 0 t

  let norm t =
    let rec norm stack depth t =
      match t with
      | Abs(t) ->
          begin
            match stack with
            | v :: stack -> norm stack depth (subst v t)
            | []         -> let d = depth + 1 in
                            Abs(norm [] d (subst (Var(d)) t))
          end
      | App(t,u) -> norm (u :: stack) depth t
      | Var(d)   ->
          let head = Idx(depth - d) in
          List.fold_left (fun t c -> App(t, norm [] depth c)) head stack
      | Idx(_)   -> failwith "norm: open term"
    in
    norm [] 0 t

  let pp_term : out_channel -> term -> unit = fun oc t ->
    let pp fmt = Printf.fprintf oc fmt in
    let paren_if b = if b then ("(", ")") else ("", "") in
    let app_lvl = 2 in
    let abs_lvl = 1 in
    let ini_lvl = 0 in
    let rec print env nv b _ t =
      match t with
      | App(t,u) ->
          let (l, r) = paren_if (b >= app_lvl) in
          pp "%s%a %a%s" l (print env nv abs_lvl) t (print env nv app_lvl) u r
      | Abs(_)   ->
          let (l, r) = paren_if (b >= abs_lvl) in
          let rec pp_abs env nv _ t =
            match t with
            | Abs(t) -> pp " x%i%a" nv (pp_abs (nv :: env) (nv + 1)) t
            | _      -> pp ".%a" (print env nv ini_lvl) t
          in
          pp "%sλ%a%s" l (pp_abs env nv) t r
      | Idx(n)   ->
          pp "x%i" (List.nth env n)

      | Var(_)   -> failwith "pp_term: open term"
    in
    print [] 0 ini_lvl oc t
end

module Simple3 : Lambda = struct
  type term_aux =
    | Abs of bool (* occurs? *) * term_
    | Idx of int
    | App of term_ * term_
    | Var of int

  and term_ = int (* number of free variables *) * term_aux

  let subst t u =
    let rec fn d (n,u as t0) =
      if n = 0 then t0 else
      match u with
      | Abs (b,t) ->
          let t' = fn (d+1) t in
          if t' != t then n-1, Abs(b,t') else t0
      | App(t1,t2) ->
          let t1' = fn d t1 and t2' = fn d t2 in
          if t1' != t1 || t2' != t2 then n-1, App(t1',t2') else t0
      | Idx n -> if n = d then t else t0
      | Var _ -> t0
    in
    fn 0 u

  type term = S.t * term_

  let app (s1,t1) (s2,t2) =
    let ns = S.union s1 s2 in
    (ns, (S.cardinal ns, App(t1,t2)))

  (* lifting of a set (and remove variable 0) *)
  let lift s =
    S.fold (fun x s -> if x > 0 then S.add (x - 1) s else s) s S.empty

  let abs (s1,t1) =
    let ns = lift s1 in
    (ns, (S.cardinal ns, Abs(S.mem 0 s1,t1)))

  let idx n =
    let ns = S.add n S.empty in
    ns, (S.cardinal ns, Idx n)

  let norm (_, t) =
    let rec norm stack depth (_,t) =
      match t with
      | Abs(b,t) ->
          begin
            match stack with
            | v :: stack -> norm stack depth (subst v t)
            | []         ->
                let d = depth + 1 in
                abs (norm [] d (if b then subst (0, Var(d)) t else t))
          end
      | App(t,u) -> norm (u :: stack) depth t
      | Var(d)   ->
          let head = idx (depth - d) in
          List.fold_left (fun t c -> app t (norm [] depth c)) head stack
      | Idx(_)   -> failwith "norm: open term"
    in
    norm [] 0 t

  let pp_term : out_channel -> term -> unit = fun oc (_, t) ->
    let pp fmt = Printf.fprintf oc fmt in
    let paren_if b = if b then ("(", ")") else ("", "") in
    let app_lvl = 2 in
    let abs_lvl = 1 in
    let ini_lvl = 0 in
    let rec print env nv b _ t =
      match snd t with
      | App(t,u) ->
          let (l, r) = paren_if (b >= app_lvl) in
          pp "%s%a %a%s" l (print env nv abs_lvl) t (print env nv app_lvl) u r
      | Abs(_,_) ->
          let (l, r) = paren_if (b >= abs_lvl) in
          let rec pp_abs env nv _ t =
            match snd t with
            | Abs(_,t) -> pp " x%i%a" nv (pp_abs (nv :: env) (nv + 1)) t
            | _        -> pp ".%a" (print env nv ini_lvl) t
          in
          pp "%sλ%a%s" l (pp_abs env nv) t r
      | Idx(n)   ->
          pp "x%i" (List.nth env n)

      | Var(_)   -> failwith "pp_term: open term"
    in
    print [] 0 ini_lvl oc t
end

module Simple4 : Lambda = struct
  type term_ =
    | Abs of int (* # free vars *) * bool (* occurs? *) * term_
    | Idx of int
    | App of int (* # free vars *) * term_ * term_
    | Var of int

  type term = S.t * term_
  
  let get_num t =
    match t with
    | Abs(n,_,_)
    | App(n,_,_) -> n
    | Idx(_)     -> 1
    | Var(_)     -> 0
  
  let msubst lt nb t0 =
    let rec msubst d t0 =
      let n = get_num t0 in
      if n = 0 then t0 else
      match t0 with
      | Abs(_,b,t) ->
          let new_t = msubst (d + 1) t in
          if new_t != t then Abs(n - 1, b, new_t) else t0
      | App(_,t,u) ->
          let new_t = msubst d t in
          let new_u = msubst d u in
          if new_t != t || new_u != u then App(n - 1, new_t, new_u) else t0
      | Idx(n)     -> if n - d < nb then List.nth lt (n - d) else t0
      | Var(_)     -> t0
    in
    if nb = 0 then t0 else msubst 0 t0
  
  let app (s1,t1) (s2,t2) =
    let ns = S.union s1 s2 in
    (ns, App(S.cardinal ns, t1, t2))
  
  (* lifting of a set (and remove and tell if the variable 0 occurs) *)
  let lift s =
    let b = ref false in
    let fn x s = if x > 0 then S.add (x-1) s else (b:= true; s) in
    let ns = S.fold fn s S.empty in
    (!b, ns)
  
  let abs (s1,t1) =
    let b, ns = lift s1 in
    (ns, Abs(S.cardinal ns, b, t1))
  
  let idx n =
    let ns = S.add n S.empty in
    ns, Idx n
  
  let norm t =
    let rec norm stack depth t =
      match t with
      | Abs(_,_,_) ->
          let rec gn lt nb b depth stack u =
            match (stack, u) with
            | (t :: stack, Abs(_,b',u')) ->
                gn (t :: lt) (nb + 1) (b || b') depth stack u'
            | ([]        , Abs(_,b',u')) ->
                let d = depth + 1 in
                abs (gn (Var(d)::lt) (nb+1) (b || b') d [] u')
            | _                           ->
                norm stack depth (if b then msubst lt nb u else u)
          in
          gn [] 0 false depth stack t
      | App(_,t,u) -> norm (u :: stack) depth t
      | Var(d)     ->
          let head = idx (depth - d) in
          List.fold_left (fun t c -> app t (norm [] depth c)) head stack
      | Idx(_)     -> failwith "bug in norm_left"
    in
    snd (norm [] 0 t)

  let norm (s, t) = assert (S.is_empty s); (s, norm t)
  
  let pp_term : out_channel -> term -> unit = fun oc (_, t) ->
    let pp fmt = Printf.fprintf oc fmt in
    let paren_if b = if b then ("(", ")") else ("", "") in
    let app_lvl = 2 in
    let abs_lvl = 1 in
    let ini_lvl = 0 in
    let rec print env nv b _ t =
      match t with
      | App(_,t,u) ->
          let (l, r) = paren_if (b >= app_lvl) in
          pp "%s%a %a%s" l (print env nv abs_lvl) t (print env nv app_lvl) u r
      | Abs(_,_,_) ->
          let (l, r) = paren_if (b >= abs_lvl) in
          let rec pp_abs env nv _ t =
            match t with
            | Abs(_,_,t) -> pp " x%i%a" nv (pp_abs (nv :: env) (nv + 1)) t
            | _          -> pp ".%a" (print env nv ini_lvl) t
          in
          pp "%sλ%a%s" l (pp_abs env nv) t r
      | Idx(n)   ->
          pp "x%i" (List.nth env n)

      | Var(_)   -> failwith "pp_term: open term"
    in
    print [] 0 ini_lvl oc t
end

module Test(L: Lambda) = struct
  include L

  let zero : term = abs (abs (idx 0))
  let succ = abs (abs (abs (app (idx 1) (app (app (idx 2) (idx 1)) (idx 0)))))
  let plus = abs (abs (abs (abs (app (app (idx 3) (idx 1))
                                     (app (app (idx 2) (idx 1)) (idx 0))))))
  let mult = abs (abs (abs (app (idx 2) (app (idx 1) (idx 0)))))
  let pred =
    abs (app (app (app (app (idx 0)
                            (abs (abs (abs (app (app (idx 2)
                                                     (app succ (idx 1)))
                                                (idx 1))))))
                       zero)
                  zero)
             zero)

  let nat_2    = app succ (app succ zero)
  let nat_4    = app nat_2 nat_2
  let nat_8    = app (app plus nat_4) nat_4
  let nat_10   = app (app plus nat_2) nat_8
  let nat_100  = app (app mult nat_10) nat_10
  let nat_400  = app (app mult nat_4) nat_100
  let nat_1000 = app (app mult nat_100) nat_10
  let nat_4000 = app (app mult nat_4) nat_1000

  let t0 = app (app nat_4000 pred) nat_4000
  let t1 = app (app mult nat_400) nat_1000

  let run () =
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
end

module T1 = Test(Simple1)

let _ =
  let error () = Printf.eprintf "Usage: %s N\n%!" Sys.argv.(0); exit 1 in
  let n =
    match Sys.argv with
    | [|_; n|] -> (try int_of_string n with Failure(_) -> error ())
    | _        -> error ()
  in
  let (module L : Lambda) =
    match n with
    | 1 -> (module Simple1)
    | 2 -> (module Simple2)
    | 3 -> (module Simple3)
    | 4 -> (module Simple3)
    | _ -> error ()
  in
  let module T = Test(L) in
  T.run ()
