
type term'  =
  Abs of bool * term      (* abstraction: the boolean
                             tell if the variable occur or not *)
| BVar of int             (* bound variable *)
| App of term * term
| FVar of int	          (* free variable, used to normalise under lambdas *)
and term = int * term'    (* this integer keeps the number of free variable *)

(* The substitution *)

let subst t u = let rec fn d (n,u as t0) =
  if n = 0 then t0 else match u with
  Abs (b,t) ->
    let t' = fn (d+1) t in
    if t' != t then n-1, Abs(b,t') else t0
| App(t1,t2) ->
    let t1' = fn d t1 and t2' = fn d t2 in
    if t1' != t1 || t2' != t2 then n-1, App(t1',t2') else t0
| BVar n -> if n = d then t else t0
| FVar _ -> t0
in fn 0 u


(* To construct term, we need to manipulate set of variables (integer) *)
module Int_ord = struct
  type t = int
  let compare = ((-) : int -> int -> int)
end;;
module S = Set.Make(Int_ord);;

(* bApp take two pairs (Set_of_var in t, t) and build there application *)
let bApp (s1,t1) (s2,t2) =
  let ns = S.union s1 s2 in
  (ns, (S.cardinal ns, App(t1,t2)))

(* Application of two close term *)
let cApp t1 t2 =
  0, App(t1,t2)

(* lifting of a set (and remove variable 0) *)
let lift s =
  S.fold (fun x s -> if x > 0 then S.add (x-1) s else s) s S.empty

(* bAbs take one pair (Set_of_var in t, t) and build its abstraction *)
let bAbs (s1,t1) =
  let ns = lift s1 in
  (ns, (S.cardinal ns, Abs(S.mem 0 s1,t1)))

(* building of a bound variable *)
let bVar n =
  let ns = S.add n S.empty in
  ns, (S.cardinal ns, BVar n)

(* building of a closed term *)
let bClos t =
  S.empty, t

(* The normalisation (in call by name) : a stack no environment *)
let norm t = let rec fn stack depth (_,t) = match t with
    Abs(true,t) ->
      begin
        match stack with
          [] ->
           let depth' = depth+1 in
           bAbs (fn [] depth' (subst (0, FVar depth') t))
        | c::stack' ->
           fn stack' depth (subst c t)
      end
  | Abs(false,t) ->
      begin
        match stack with
          [] ->
           let depth' = depth+1 in
           bAbs(fn [] depth' t)
        | c::stack' ->
           fn stack' depth t
      end
  | App(t1,t2) ->
      fn (t2::stack) depth t1
  | FVar depth' ->
      let head = bVar (depth-depth') in
      List.fold_left (fun t c -> bApp t (fn [] depth c)) head stack
  | BVar _ -> failwith "bug in norm_left"
in snd (fn [] 0 t)

let rec nth n l = match n, l with
  _, [] -> raise (Invalid_argument "nth")
| 0, (c::_) -> c
| n, (_::l) -> nth (n-1) l
;;

(* printing of term *)

let app_lvl = 2
let abs_lvl = 1
let ini_lvl = 0

let print_term = let rec fn env nv b (_,t as t0) = match t with
  App(t1,t2) ->
    if b >= app_lvl then print_string "(";
    fn env nv abs_lvl t1;
    print_string " ";
    fn env nv app_lvl t2;
    if b >= app_lvl then print_string ")"
| Abs(_,_) ->
    if b >= abs_lvl then print_string "(";
    print_string "\\";
    let rec gn env nv = function
      _, Abs (_,t) ->
        let name = "x"^(string_of_int nv) in
        print_string name;
        print_string " ";
        gn (name::env) (nv + 1) t
    | t ->
        print_string "-> ";
        fn env nv ini_lvl t
    in gn env nv t0;
    if b >= abs_lvl then print_string ")"
| BVar n ->
    print_string (nth n env)
| FVar n ->
    print_string ("y"^(string_of_int n))
in fn [] 0 ini_lvl

let zero = snd
   (bAbs (bAbs (bVar 0)))

let succ = snd
   (bAbs (bAbs (bAbs (bApp (bVar 1)
                  (bApp (bApp (bVar 2) (bVar 1))
                             (bVar 0))))))

let plus = snd (bAbs (bAbs (bAbs (bAbs (bApp
                 (bApp (bVar 3) (bVar 1))
             (bApp
                 (bApp (bVar 2) (bVar 1)) (bVar 0)))))))

let mul = snd (bAbs (bAbs (bAbs (bApp (bVar 2) (bApp (bVar 1) (bVar 0))))))

let pred =  snd (bAbs
  (bApp (bApp (bApp (bApp (bVar 0)
    (bAbs (bAbs (bAbs (bApp
      (bApp (bVar 2) (bApp (bClos succ) (bVar 1))) (bVar 1))))))
    (bAbs (bAbs (bVar 0)))) (bClos zero)) (bClos zero)))


let two = cApp(succ) (cApp(succ) (zero))
let four = cApp(two) (two)
let height = cApp(cApp(plus) (four)) (four)
let ten = cApp(cApp(plus) (two)) (height)
let hundred = cApp(cApp(mul) (ten)) (ten)
let thousand = cApp(cApp(mul) (hundred)) (ten)

let bench () =
  let fh = cApp (cApp mul four) hundred in
  let ft = cApp (cApp mul four) thousand in
  print_term (norm (cApp (cApp ft pred) ft));
  print_newline();
  print_term (norm (cApp (cApp mul fh) thousand));
  print_newline();
  Printf.eprintf "top heap: %d\n%!" Gc.((stat ()).top_heap_words)

let _ = bench ()
