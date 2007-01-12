
type term  = (* this integer in App and Abs keeps 
                the number of free variable *)
  Abs of int * bool * term      (* abstraction: the boolean 
                             tell if the variable occur or not *)
| BVar of int             (* bound variable *)
| App of int * term * term
| FVar of int	          (* free variable, used to normalise under lambdas *)

let get_num = function
  Abs(n,_,_) -> n
| App(n,_,_) -> n
| BVar _ -> 1
| FVar _ -> 0
 
let rec nth n l = match n, l with
  _, [] -> raise (Invalid_argument "nth")
| 0, (c::_) -> c
| n, (_::l) -> nth (n-1) l

(* The substitution *)

let msubst lt nb t0 = 
  let rec fn d t0 = 
    let n = get_num t0 in
    if n = 0 then t0 else match t0 with
    Abs (_,b,t) -> 
      let t' = fn (d+1) t in 
      if t' != t then Abs(n-1,b,t') else t0
  | App(_,t1,t2) ->
      let t1' = fn d t1 and t2' = fn d t2 in
      if t1' != t1 or t2' != t2 then App(n-1,t1',t2') else t0
  | BVar n -> if n - d < nb then nth (n-d) lt else t0
  | FVar _ -> t0
  in if nb = 0 then t0 else fn 0 t0

(* To construct term, we need to manipulate set of variables (integer) *)
module Int_ord = struct 
  type t = int
  let compare = ((-) : int -> int -> int)
end;;
module S = Set.Make(Int_ord);;

(* bApp take two pairs (Set_of_var in t, t) and build there application *)
let bApp (s1,t1) (s2,t2) =
  let ns = S.union s1 s2 in
  (ns, App(S.cardinal ns, t1, t2))

(* Application of two close term *)
let cApp t1 t2 =
  App(0, t1, t2)

(* lifting of a set (and remove and tell if the variable 0 occurs) *)
let lift s =
  let b = ref false in
  let ns = 
    S.fold (fun x s -> if x > 0 then S.add (x-1) s 
                                else (b:= true; s)) 
           s S.empty
  in !b , ns

(* bAbs take one pair (Set_of_var in t, t) and build its abstraction *)
let bAbs (s1,t1) = 
  let b, ns = lift s1 in
  (ns, Abs(S.cardinal ns, b, t1))

(* building of a bound variable *)
let bVar n =
  let ns = S.add n S.empty in
  ns, BVar n

(* building of a closed term *)
let bClos t =
  S.empty, t

(* The normalisation (in call by name) : a stack no environment *)
let norm t0 = let rec fn stack depth = function
    Abs _ as t ->
      let rec gn lt nb b depth stack u = match stack, u with
        (t::stack'), (Abs(_,b',u')) ->
          gn (t::lt) (nb+1) (b or b') depth stack' u'
      | [], (Abs(_,b',u')) ->
          let depth' = depth + 1 in
          bAbs (gn (FVar depth'::lt) (nb+1) (b or b') depth' [] u')
      | _ ->
          fn stack depth (if b then msubst lt nb u else u)
      in gn [] 0 false depth stack t
  | App(_,t1,t2) -> 
      fn (t2::stack) depth t1
  | FVar depth' ->
      let head = bVar (depth-depth') in
      List.fold_left (fun t c -> bApp t (fn [] depth c)) head stack 
  | BVar _ -> failwith "bug in norm_left"
in snd (fn [] 0 t0)

(* printing of term *)

let app_lvl = 2
let abs_lvl = 1
let ini_lvl = 0

let print_term = let rec fn env nv b = function
  App(_,t1,t2) ->
    if b >= app_lvl then print_string "(";
    fn env nv abs_lvl t1;
    print_string " ";
    fn env nv app_lvl t2;
    if b >= app_lvl then print_string ")"
| Abs(_,_,_) as t0 ->
    if b >= abs_lvl then print_string "(";
    print_string "\\";
    let rec gn env nv = function
      Abs (_,_,t) ->
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


let two = cApp succ (cApp succ zero)
let four = cApp(two) (two)
let height = cApp(cApp(plus) (four)) (four)
let ten = cApp(cApp(plus) (two)) (height)
let hundred = cApp(cApp(mul) (ten)) (ten)
let thousand = cApp(cApp(mul) (hundred)) (ten)


let bench () =
  let fh = cApp(cApp(mul) (four)) (hundred) in
  print_term (norm (cApp(cApp(fh) (pred)) (fh)));
  print_newline();
  print_term (norm (cApp(cApp(mul) (ten)) (thousand)));
  print_newline()

let _ = 
  bench ()

