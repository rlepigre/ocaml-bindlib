type term =
  Abs of term
| BVar of int
| App of term * term
;;

type closure =
  Clos of term * env
| FVar of int

and env = closure list
;;

let rec nth n l = match n, l with
  _, [] -> raise (Invalid_argument "nth")
| 0, (c::_) -> c
| n, (_::l) -> nth (n-1) l
;;

let norm t = let rec 
  fn (env:env) (stack:env) depth = function
    Abs t -> 
      begin
        match stack with
          [] -> 
           let depth' = depth+1 in
           Abs(fn (FVar depth'::env) stack (* = []*) depth' t)
        | c::stack' -> 
           fn (c::env) stack' depth t
      end
(* next case is an optimisation ! and can be removed. but don't do it *)
  | App(t1,BVar n) -> 
      fn env ((nth n env)::stack) depth t1
  | App(t1,t2) -> 
      fn env (Clos(t2,env)::stack) depth t1
  | BVar n ->
      begin 
        let rec gn stack = function
          Clos(t,env') -> 
            fn env' stack depth t
        | FVar depth' -> 
            let head = BVar (depth-depth') in
            List.fold_left (fun t c -> App(t,gn [] c)) head stack 
        in gn stack (nth n env) 
      end 
in fn [] [] 0 t 
;;

(* printing of term *)

let app_lvl = 2;;
let abs_lvl = 1;;
let ini_lvl = 0;;

let print_term = let rec fn env nv b = function
  App(t1,t2) ->
    if b >= app_lvl then print_string "(";
    fn env nv abs_lvl t1;
    print_string " ";
    fn env nv app_lvl t2;
    if b >= app_lvl then print_string ")"
| Abs _ as t ->
    if b >= abs_lvl then print_string "(";
    print_string "\\";
    let rec gn env nv = function
      Abs t ->
        let name = "x"^(string_of_int nv) in
        print_string name;
        print_string " ";
        gn (name::env) (nv + 1) t
    | t -> 
        print_string "-> ";
        fn env nv ini_lvl t
    in gn env nv t; 
    if b >= abs_lvl then print_string ")"
| BVar n ->
    print_string (nth n env)
in fn [] 0 ini_lvl 
;;

let zero = 
   (Abs (Abs (BVar 0)));;

let succ = 
   (Abs (Abs (Abs (App (BVar 1, 
                  App (App (BVar 2,BVar 1),
                             BVar 0))))));;

let plus = (Abs (Abs (Abs (Abs (App 
                 (App (BVar 3, BVar 1),  
             App 
                 (App (BVar 2,BVar 1),BVar 0)))))));;

let mul = (Abs (Abs (Abs (App (BVar 2, App (BVar 1,BVar 0))))));;

let pred =  (Abs
  (App (App (App (App (BVar 0,
    Abs (Abs (Abs (App 
      (App (BVar 2, App (succ, BVar 1)), BVar 1))))), 
    Abs (Abs (BVar 0))), zero), zero)))
;;


let two = App(succ,App(succ,zero));;
let four = App(two,two);;
let height = App(App(plus,four),four);;
let ten = App(App(plus,two),height);;
let hundred = App(App(mul,ten),ten);;
let thousand = App(App(mul,hundred),ten);;


let bench () =
  let fh = App(App(mul,four),hundred) in
  print_term (norm (App(App(fh,pred),fh)));
  print_newline();
  print_term (norm (App(App(mul,ten),thousand)));
  print_newline()
;;

bench ();;

