open Bindlib

module Lambda = struct
  type term = {
    address: int;
    key:int;
    data:term';
  }
	     
  and term' =
    | HVar of int
    | Var of term variable
    | App of term * term
    | Lam of (term, term) binder * int option

  type t = term

  let mkHVar n = { address = -n; key = -n; data = HVar n }
 		   
  let hash' = function
    | HVar n -> -n
    | Var v -> hash_var v
    | App(t1, t2) -> t1.key * 31 + t2.key
    | Lam(f,h) ->
       let n = match h with None -> 0 | Some n -> n in 91 * (subst f (mkHVar n)).key + 3
											 
  let equal' t1 t2 = t1 == t2 || match t1, t2 with
    | HVar n1, HVar n2 -> n1 = n2
    | Var v1, Var v2 -> compare_variables v1 v2 = 0
    | App(t1,t2), App(t1',t2') ->
       t1.address = t1'.address && t2.address = t2'.address
    | Lam(f, h), Lam(f', h') ->
       h = h' &&
	 let n = match h with None -> 0 | Some n -> n in
	 (subst f (mkHVar n)).address = (subst f' (mkHVar n)).address
    | _ -> false

  let hash r = r.key

  let equal r1 r2 = equal' r1.data r2.data
end
		  
module WLambda = Weak.Make(Lambda)

include Lambda

let hashtbl = WLambda.create 1001
let get_addr =
  let c = ref 1 in
  fun () ->
    let c' = !c in
    c := c' + 1;
    c'

let hashCons' data =
  let key = hash' data in
  let address = get_addr () in
  let data' = { address; key; data } in
  try WLambda.find hashtbl data'
  with Not_found ->
    WLambda.add hashtbl data'; data'

      
let rec print ch t =
  match t.data with
    Lam(f,h) ->
    let v = new_var (fun v -> hashCons' (Var v)) (binder_name f) in
    Printf.fprintf ch "λ%s.%a" (name_of v) print (subst f (free_of v)) 
  | App(t1,t2) ->
    Printf.fprintf ch "(%a %a)" print t1 print t2
  | Var v -> Printf.fprintf ch "%s" (name_of v)
  | HVar n -> Printf.fprintf ch "HVar(%d)" n


let hashCons data =
  let key = hash' data in
  let address = get_addr () in
  let data' = { address; key; data } in
  (*  Printf.eprintf "Hashing: %a ⇒ " print data';*)
  try
    let res = WLambda.find hashtbl data' in
    (*Printf.eprintf "Found %a\n%!" print res;*)
    res
  with Not_found ->
    (*Printf.eprintf "Not found\n%!";*)
    WLambda.add hashtbl data'; data'

let mkApp(t1, t2) =
  unit_apply hashCons (App(^t1,t2^) )

let mkApp2(t1,t2) = hashCons(App(t1,t2))
			    
let mkLam f = 	     
  unit_apply hashCons (let lam (f,h) = Lam(f,h) in unit_apply lam (bind_info (fun _ -> assert false) "x" f) )

let rec eval t =
  Printf.eprintf "%a\n%!" print t;
  match t.data with
    Lam _ -> t
  | App(t1,t2) ->
     begin
       let t2 = eval t2 in
       match (eval t1).data with
	 Lam(f,_) -> eval (subst f t2)
       | _ -> failwith "untyped"
     end
  | Var _ -> failwith "open term"
  | HVar _ -> failwith "open term (H)"


let idt = unbox (mkLam (fun x -> x))
let delta = unbox (mkLam (fun x -> mkApp(x,x)))
let idt' = unbox (mkLam (fun x -> x))
let delta' = unbox (mkLam (fun x -> mkApp(x,x)))		  
let two = unbox (mkLam (fun f -> mkLam (fun x -> mkApp(f,mkApp(f,x)))))
let add = unbox (mkLam (fun n -> mkLam (fun m -> mkLam (fun f -> mkLam (fun x -> mkApp(mkApp(n,f),mkApp(mkApp(m,f),x)))))))
let zero = unbox (mkLam (fun f -> mkLam (fun x -> x)))
let succ = unbox (mkLam (fun n -> mkLam (fun f -> mkLam (fun x -> mkApp(f,mkApp(mkApp(n,f),x))))))
let dog = eval (mkApp2(mkApp2(mkApp2(two,two),succ),zero))
let four = eval (mkApp2(mkApp2(mkApp2(mkApp2(add,two),two),succ),zero))
		 
let _ = assert (idt.address = idt'.address)
let _ = assert (idt.data == idt'.data)

let _ = assert (delta.address = delta'.address &&
		  delta.data == delta'.data)
	       
let idt'' = eval (mkApp2(delta, mkApp2(delta', idt)))

let _ =
  Printf.eprintf "%a\n%!" print idt; 
  Printf.eprintf "%a\n%!" print idt'; 
  Printf.eprintf "%a\n%!" print delta; 
  Printf.eprintf "%a\n%!" print delta'; 
  Printf.eprintf "%a\n%!" print idt''; 
  Printf.eprintf "%a\n%!" print two;
  Printf.eprintf "%a\n%!" print four;
  Printf.eprintf "%a\n%!" print dog 

let _ = assert (idt.address = idt''.address)
let _ = assert (idt.data == idt''.data)

let _ = assert (four.address = dog.address)
let _ = assert (four.data == dog.data)
