open Bindlib
open Earley

module Lambda = struct
  type term = {
    mutable address: int;
    mutable key:int;
    data:term';
  }

  and term' =
    | HVar of int

    | Var of term var

    | App of term * term
    | Lam of (term, term) binder

    | Con of string * term
    | Case of term * (string * (term, term) binder) list * (term, term) binder option

(*
  { a = t; b = u } ⟹ fun x -> case x with A[k] -> k t  | B[k] -> k u
  { a : tₐ; b : t_b } ⟹ [A[tₐ → ⊥] | B[t_b → ⊥]] -> ⊥
  k s.a ⟹ s A[k]

 *)

    | Struct of term option * (string * (term, term) binder) list
    | VStruct of (string, term) Hashtbl.t
    | Proj of term * string

(*
    manque open … in
    les coercions de type
    8<
    Mu/Stack
*)

  type t = term

  let mkHVar n = { address = -n; key = Hashtbl.hash (`HVar, n); data = HVar n }

  let canonical_var f =
    let n = if binder_occur f then binder_rank f + 1 else 0 in
    mkHVar n

  let hash_binder f =
    Hashtbl.hash(binder_occur f,(subst f (canonical_var f)).key)

  let hash' = function
    | HVar n -> Hashtbl.hash (`HVar, n)
    | Var v -> hash_var v
    | App(t1, t2) -> Hashtbl.hash (`App,t1.key,t2.key)
    | Lam(f) -> Hashtbl.hash(`Lam, hash_binder f)
    | Con(str,t1) ->
       Hashtbl.hash(`Con, t1.key)
    | Case(t, cases, any) ->
       let any = match any with None -> None | Some f -> Some (hash_binder f) in
       let cases = List.map (fun (str, f) -> (str, hash_binder f)) cases in
       Hashtbl.hash(`Case,t.key,cases,any)
    | Proj(t1,str) ->
       Hashtbl.hash(`Proj, t1.key)
    | Struct(any, cases) ->
       let any = match any with None -> None | Some f -> Some f.key in
       let cases = List.map (fun (str, f) -> (str, hash_binder f)) cases in
       Hashtbl.hash(`Struct,cases,any)
    | VStruct(cases) ->
       let cases = Hashtbl.fold (fun str f l -> (str, f.key)::l) cases [] in
       let cases = List.sort (fun (s,_) (s',_) -> compare s s') cases in
       Hashtbl.hash(`VStruct,cases)

  let equal_binder f f' =
    binder_rank f = binder_rank f' && binder_occur f = binder_occur f' &&
      (subst f (canonical_var f)).address = (subst f' (canonical_var f)).address

  let equal' t1 t2 = t1 == t2 || match t1, t2 with
    | HVar n1, HVar n2 -> n1 = n2
    | Var v1, Var v2 -> compare_vars v1 v2 = 0
    | App(t1,t2), App(t1',t2') ->
       t1.address = t1'.address && t2.address = t2'.address
    | Lam(f), Lam(f') -> equal_binder f f'
    | Con(str,arg), Con(str',arg') ->
       str = str' && arg.address = arg'.address
    | Case(t,cases,any), Case(t',cases',any') ->
       t.address = t'.address &&
         List.length cases = List.length cases' &&
           List.for_all2 (fun (s,f) (s',f') -> s = s' && equal_binder f f') cases cases' &&
             (match any, any' with
                None, None -> true
              | Some f, Some f' -> equal_binder f f'
              | _ -> false)
    | Proj(arg,str), Proj(arg',str') ->
       str = str' && arg.address = arg'.address
    | Struct(any,cases), Struct(any',cases') ->
         List.length cases = List.length cases' &&
           List.for_all2 (fun (s,f) (s',f') -> f = f' && equal_binder f f') cases cases' &&
             (match any, any' with
                None, None -> true
              | Some f, Some f' -> f.address = f'.address
              | _ -> false)
    | VStruct(cases), VStruct(cases') ->
       let cases = Hashtbl.fold (fun str f l -> (str, f)::l) cases [] in
       let cases = List.sort (fun (s,_) (s',_) -> compare s s') cases in
       let cases' = Hashtbl.fold (fun str f l -> (str, f)::l) cases' [] in
       let cases' = List.sort (fun (s,_) (s',_) -> compare s s') cases' in
       List.length cases = List.length cases' &&
       List.for_all2 (fun (s,f) (s',f') -> s = s' && f.address = f'.address) cases cases'
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

let reHashCons old =
  old.key <- hash' old.data;
  old.address <- get_addr ();
  (*  Printf.eprintf "Hashing: %a ⇒ " print data';*)
  try
    let res = WLambda.find hashtbl old in
    (*Printf.eprintf "Found %a\n%!" print res;*)
    old.address <- get_addr ();
    res
  with Not_found ->
    (*Printf.eprintf "Not found\n%!";*)
    WLambda.add hashtbl old; old

let rec print ch t =
  match t.data with
    Lam(f) ->
    let v = new_var (fun v -> hashCons (Var v)) (binder_name f) in
    Printf.fprintf ch "λ%s.%a" (name_of v) print (subst f (free_of v))
  | App(t1,t2) ->
    Printf.fprintf ch "(%a %a)" print t1 print t2
  | Var v -> Printf.fprintf ch "%s" (name_of v)
  | HVar n -> Printf.fprintf ch "HVar(%d)" n
  | Con(str,t) -> Printf.fprintf ch "%s[%a]" str print t
  | Case(t,cases,any) -> Printf.fprintf ch "case %a of …" print t
  | Struct(any,cases) -> Printf.fprintf ch "{…}"
  | VStruct(cases) ->
     Printf.eprintf "{";
     Hashtbl.iter (fun str t -> Printf.eprintf "%s = %a; " str print t) cases;
     Printf.eprintf "}"
  | Proj(t,str) -> Printf.fprintf ch "(%a).%s" print t str

let var v = hashCons (Var v)

let mkApp(t1, t2) =
  box_apply hashCons (box_apply2 (fun t1 t2 -> App(t1,t2)) t1 t2 )

let mkApp2(t1,t2) = hashCons(App(t1,t2))

let mkLam(name, f) =
  box_apply hashCons (box_apply (fun x -> Lam x) (bind var name f ) )

let mkCon(str,t) =
  box_apply hashCons (box_apply2 (fun s t -> Con(s,t)) (box str) t)

let mkCon2(str,t) =
  hashCons (Con(str, t))

let mkCase(t,cases,any) =
  box_apply hashCons
             (box_apply3 (fun t c a -> Case(t,c,a)) t
                          (box_list ((List.map (fun (str,f) -> box_pair (box str) (bind var "c" f))) cases))
                          (match any with None -> box None | Some f -> (box_apply (fun x -> Some x) (bind var "d" f))))

let mkProj(t,str) =
  box_apply hashCons (box_apply2 (fun s t -> Proj(t,s)) (box str) t)

let mkProj2(t,str) =
  hashCons (Proj(t,str))

let mkStruct(self,any,cases) =
  box_apply hashCons
             (box_apply2 (fun a c -> Struct(a,c))
                          (match any with None -> box None | Some a -> (box_apply (fun x -> Some x) a))
                          (box_list ((List.map (fun (str,f) -> box_pair (box str) (bind var self f))) cases)))

let mkUnit = mkStruct("_",None, [])

let atom_term = declare_grammar "atom_term"
let term = declare_grammar "term"
let kwds = [ "case"; "of"; "let" ]
let ident = parser str:''[A-Za-z][A-Za-z0-9_']*'' ->
  if List.mem str kwds then give_up (); str

let _ = set_grammar atom_term (parser

      | '(' t:term ')' -> t

      | id:ident -> (fun env -> try List.assoc id env with Not_found -> give_up ())

      | "fun" l:ident+ {"->"|"→"} t:term ->
         let rec fn l env = match l with
             [] -> t env
           | id::l ->
              mkLam(id, fun x ->
                    let env = (id, x)::env in
                    fn l env)
         in
         fn l

      | str:ident '[' t:term? ']' ->
            (fun env -> let t = match t with None -> mkUnit | Some t -> t env in mkCon(str,t))

      | "case" t:term "of"
               cases:{"|" c:ident "[" i:{ident|''_''}?["_"] "]" _:{"->"|"→"} t:term}*$
               any:{"|" i:{ident|''_''} _:{"->"|"→"} t:term }? ->
         (fun env ->
          let cases = List.map (fun (c,i,t) -> (c, fun x ->

                                                   let env = (i,x)::env in t env)) cases in
          let any = match any with
            | None -> None
            | Some(i,t) -> Some (fun x -> let env = (i,x)::env in t env)
          in
          mkCase(t env,cases,any))

      | '{'
            any:{term '|' }?
            cases:{ident '=' term ';'}* '}'  self:{"as" ident}?["_"] -> (fun env ->
                  let cases = List.map (fun (i,t) -> (i, fun x ->
                     let env = (self,x)::env in t env)) cases in
                  let any = match any with
                    | None -> None
                    | Some t -> Some (t env)
                  in
                  (mkStruct(self,any,cases) : term bindbox))
      )

let proj_term = parser
      | t:atom_term l:{ '.' - ident}*$ ->
          (fun env -> List.fold_left (fun acc str -> mkProj(acc,str)) (t env) l)

let _ = set_grammar term (parser
         | t:proj_term l:proj_term*$ -> (fun env -> List.fold_left (fun t u -> mkApp(t,u env)) (t env) l))

let rec eval t =
  (*  Printf.eprintf "%a\n%!" print t;*)
  match t.data with
    Lam _ -> t
  | App(t1,t2) ->
     begin
       let t2 = eval t2 in
       match (eval t1).data with
         Lam(f) -> eval (subst f t2)
       | _ -> failwith "untyped"
     end
  | Con(str,t) ->
     let t = eval t in mkCon2(str, t)
  | Case(t,cases,any) ->
     let (str,t') =
       match (eval t).data with
       | Con(str,t') -> (str,t')
       | _           -> assert false
     in
     (try eval (subst (List.assoc str cases) t') with Not_found ->
        match any with Some f -> eval (subst f t) | _ -> assert false)
  | VStruct(cases) -> t
  | Struct(any,cases) ->
     (let tbl:(string, term) Hashtbl.t =
       match any with
       | None   -> Hashtbl.create 13
       | Some v -> let t =
                     match (eval v).data with
                     | VStruct t -> t
                     | _         -> assert false
                   in Hashtbl.copy t
      in
      (* can not hashcons before the VStruct is fully computed *)
      (* this is not correct if self is preserved under closure *)
      (* one update the key and address using reHashCons ? *)
      (* is this reasonnable ? *)
      let self:term = { key = -1; address = -1; data = VStruct tbl } in
      List.iter (fun (str,t) -> Hashtbl.add tbl str (eval (subst t self))) cases;
      reHashCons self)
  | Proj(t,str) ->
      let t =
        match (eval t).data with
        | VStruct t -> t
        | _         -> assert false
      in
      begin
        try Hashtbl.find t str with Not_found ->
          Printf.eprintf "Not found: %S\n%!" str; exit 1
      end

  | Var _ -> failwith "open term"
  | HVar _ -> failwith "open term (H)"

let blank = EarleyStr.blank_regexp ''[ \n\t\r]*''

let term_of_string s = unbox (parse_string term blank s [])
let idt = term_of_string "fun x -> x"
let delta = term_of_string "fun x -> x x"
let idt' = term_of_string "fun x -> x"
let delta' = term_of_string "fun x -> x x"
let zero = term_of_string "fun f x -> x"
let succ = term_of_string "fun n f x -> n f (f x)"
let two = term_of_string "fun f x -> f (f x)"
let add = term_of_string "fun n m f x -> n f (m f x)"

let test = term_of_string "case S[Z[]] of | Z[] -> A[] | S[n] -> B[n]"

let gros = term_of_string "{ zero = Z[]; \
                             one = S[self.zero]; \
                             add = fun n m -> case n of | Z[] -> m | S[n'] -> S[self.add n' m]; \
                             two = self.add self.one self.one; \
                             mul = fun n m -> case n of | Z[] -> Z[] | S[n'] -> self.add m (self.mul n' m); \
                             four = self.mul self.two self.two; \
                             fact = fun n -> case n of | Z[] -> self.one | S[n'] -> self.mul n (self.fact n'); \
                             test = self.fact self.four; \
                           } as self"
(*


  two = self.add self.one self.one;
} as self*)

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
  Printf.eprintf "%a\n%!" print dog;
  Printf.eprintf "%a\n%!" print test;
  Printf.eprintf "%a\n%!" print (eval test);
  Printf.eprintf "%a\n%!" print gros;
  Printf.eprintf "%a\n%!" print (eval gros)

let _ = assert (idt.address = idt''.address)
let _ = assert (idt.data == idt''.data)

let _ = assert (four.address = dog.address)
let _ = assert (four.data == dog.data)
