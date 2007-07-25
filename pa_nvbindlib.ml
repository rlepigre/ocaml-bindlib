open Camlp4.PreCast

module Id : Camlp4.Sig.Id =
    struct
      let name = "bindlib"
      let version = "3.2"
    end

module Extension (Syntax : Camlp4.Sig.Camlp4Syntax) =
	struct
include Syntax


let rec mklistupexp loc top = function
  | [] -> <:expr@loc< Nvbindlib.unit [] >>
  | e1::el -> 
      <:expr@loc< Nvbindlib.unit_apply2 (fun x y -> [ x::y ]) $e1$ $mklistupexp loc false el$ >>  

let mktuppleupexp loc el =
  match el with 
    [] ->
      <:expr@loc< Nvbindlib.unit () >>
  | [e] ->
      <:expr@loc< Nvbindlib.unit $e$ >>
  | _ ->
      let et, preambule, preambule2, _ =
	List.fold_left 
	  (fun (et,ex,el,i) e ->
	    let namex = "x#"^string_of_int i in
	    let namey = "y#"^string_of_int i in
	    (match et with
	      Some et -> Some <:expr@loc<$et$,($lid:namex$ $lid:"v#"$)>>
	    | None -> Some <:expr@loc<($lid:namex$ $lid:"v#"$)>>),
            (fun next -> 
	      ex (<:expr@loc<let ($lid:"e#"$, $lid:namey$) = special_apply $lid:"e#"$ $e$ in $next$>>)),
	    (fun next ->
	      el  (<:expr@loc<let $lid:namex$ = $lid:namey$ $lid:"h#"$ in $next$>>)),
	    i+1)
	  (None, 
	   (fun next -> <:expr@loc<let $lid:"e#"$ = special_start in $next$ >>), 
	   (fun next -> next), 
	   0)
	  el
      in 
      let et = match et with None -> <:expr@loc<()>> | Some et -> et in 
      let right = preambule2 <:expr@loc<(fun $lid:"v#"$ -> ($tup:et$))>>
      in
      preambule <:expr@loc<special_end $lid:"e#"$ (fun $lid:"h#"$ -> $right$)>>

let mkarrayupexp _loc el =
  let et, preambule, preambule2, _ =
    List.fold_left 
      (fun (et,ex,el,i) e ->
	let namex = "x#"^string_of_int i in
	let namey = "y#"^string_of_int i in
	(<:expr<$lid:namex$ $lid:"v#"$>>::et),
        (fun next -> 
	  ex (<:expr<let ($lid:"e#"$, $lid:namey$) = special_apply $lid:"e#"$ $e$ in $next$>>)),
	(fun next ->
	  el  (<:expr<let $lid:namex$ = $lid:namey$ $lid:"h#"$ in $next$>>)),
	i+1)
      ([], 
       (fun next -> <:expr<let $lid:"e#"$ = special_start in $next$ >>), 
       (fun next -> next), 
       0)
      el
  in 
  let right = preambule2 <:expr<(fun $lid:"v#"$ -> [|$list:List.rev et$|])>>
  in
  preambule <:expr<special_end $lid:"e#"$ (fun $lid:"h#"$ -> $right$)>>
  
let mkrecordupexp _loc w le = 
  let ew,start = match w with
    None -> 
      None, 
      (fun next -> <:expr<let $lid:"e#"$ = special_start in $next$ >>)
  | Some e -> 
      Some(<:expr<$lid:"w#"$ $lid:"h#"$ $lid:"v#"$>>),
      (fun next -> <:expr<let ($lid:"e#"$,$lid:"w#"$) = 
                                 (special_apply special_start $e$) in $next$ >>)
  in
  let rec rb_to_list = function
      Ast.RbNil _ -> []
    | Ast.RbSem(_,a,b) -> rb_to_list a @ rb_to_list b
    | Ast.RbEq(_,idt,e) -> [idt,e]
    | Ast.RbAnt _ -> assert false
  in
  let et, preambule, _ =
    List.fold_left 
      (fun (et,ex,i) (lbl,e) ->
	let name = "x#"^string_of_int i in
	(<:rec_binding<$lbl$ = ( $lid:name$ $lid:"h#"$ $lid:"v#"$ ) ; $et$>>),
        (fun next -> 
	  ex (<:expr<let ($lid:"e#"$, $lid:name$) = special_apply $lid:"e#"$ $e$ in $next$>>)),
	i+1)
      (<:rec_binding<>>, start, 0)
      (rb_to_list le)
  in 
  match ew with
    Some ew -> preambule <:expr<special_end $lid:"e#"$
		       (fun $lid:"h#"$ $lid:"v#"$ -> { ($ew$) with $rec_binding:et$ })>>
  | None -> preambule <:expr<special_end $lid:"e#"$
		       (fun $lid:"h#"$ $lid:"v#"$ -> { $et$ })>>


let expr1_semi_list = Gram.Entry.mk "bindlib_expr1_semi_list"
let freshin = Gram.Entry.mk "bindlib_freshin"
let vbinding = Gram.Entry.mk "bindlib_binding"
let lbl_expr_list = Gram.Entry.mk "bindlib_lbl_expr_list"
let lbl_expr = Gram.Entry.mk "bindlib_lbl_expr"

let _ =
EXTEND Gram
  expr: LEVEL "apply"
    [ [ e1 = SELF; "^^"; e2 = SELF ->
	<:expr<Nvbindlib.bind_apply $e1$ $e2$>>
    | e1 = SELF; "^|^"; e2 = SELF ->
	<:expr<Nvbindlib.mbind_apply $e1$ $e2$>>
    | e1 = SELF; "(^"; el = LIST0 expr LEVEL ":=" SEP "," ;"^)" ->
      if  Ast.is_expr_constructor e1 then begin
	let e = ref e1 in
	let n = List.length el in
	for i = 1 to n do
	  e := <:expr<$!e$ $lid:"x"^string_of_int i$>>
	done;
	for i = n downto 1 do
	  e := <:expr<fun $lid:"x"^string_of_int i$ -> $!e$>>
	done;
	match el with
	  [] -> <:expr<Nvbindlib.unit $!e$>> 
	| [e1] ->  <:expr<Nvbindlib.unit_apply $!e$ $e1$ >>
	| [e1;e2] -> <:expr<Nvbindlib.unit_apply2 $!e$ $e1$ $e2$ >>
	| e1::e2::el ->
	    let e0 = <:expr<Nvbindlib.unit_apply2 $!e$ $e1$ $e2$ >> in
	    List.fold_left (fun e1 e2 -> <:expr<Nvbindlib.apply $e1$ $e2$ >>) 
	      e0 el
      end else begin
	<:expr<$e1$ $mktuppleupexp _loc el$>>
      end ] ]
    ;
  
  expr: LEVEL "top"
    [ [
    "letvar"; fv = expr LEVEL "simple"; id = LIDENT; str = vbinding; _ = freshin;
      "in";  x = expr ->
		    <:expr<let $lid:id$ = Nvbindlib.new_var $fv$
            in $x$>>
  | "letvar"; fv = expr LEVEL "simple"; id = LIDENT; "("; n = expr LEVEL "top";")"; 
	str = vbinding; _ = freshin; "in";  x = expr ->
	    <:expr<let $lid:id$ = Nvbindlib.new_mvar $fv$
            in $x$>>
     ] ];

  match_case0: 
    [ [
    "bind"; fv = expr LEVEL "simple";  
      id = LIDENT; str = vbinding; _ = freshin;  "in"; g = LIDENT;
      "->"; f = expr -> 
	let e1 = 
	  <:expr<let $lid:g$ = Nvbindlib.subst $lid:"#e"$ (Nvbindlib.free_of $lid:id$) in $f$>> 
	in
	let e2 = 
	    <:expr<let $lid:id$ = Nvbindlib.new_var $fv$ in $e1$>> 
	in
	<:match_case<
        $lid:"#e"$ -> $e2$>>
  | "bind"; fv = expr LEVEL "simple";  
      id = LIDENT; "("; arity = LIDENT; ")"; 
	str = vbinding; _ = freshin;  "in"; g = LIDENT;
      "->"; f = expr LEVEL "top" -> 
	let e1 = 
	  <:expr<let $lid:g$ = Nvbindlib.subst $lid:"#e"$ (Nvbindlib.free_of $lid:id$) in $f$>> 
	in
	let e2 = 
	    <:expr<
	    let $lid:id$ = Nvbindlib.new_mvar $fv$ in $e1$>> 
in
	<:match_case<
	$lid:"#e"$ ->
	let $lid:arity$ = Nvbindlib.binder_arity $lid:"#e"$ in
	$e2$
	>>
     ] ];

  expr: LEVEL "~-"
    [ [
    "bindvar"; id = LIDENT; "in"; e = expr LEVEL "top" -> 
      <:expr<Nvbindlib.bind_var $lid:id$ $e$>>
  | "bindvar"; id = LIDENT; "("; ")"; "in"; e = expr LEVEL "top" -> 
      <:expr<Nvbindlib.bind_mvar $lid:id$ $e$>>
  | "bind"; fv = expr LEVEL "simple"; id = LIDENT; str = vbinding; _ = freshin; 
       "in"; e = expr LEVEL "top" -> 
	       <:expr<Nvbindlib.bind $fv$
               (fun $lid:id$ -> $e$) >>
  | "bind"; fv = expr LEVEL "simple"; id = LIDENT; "("; n = expr LEVEL "top"; ")"; 
	 str = vbinding; _ = freshin;  "in"; e = expr LEVEL "top" -> 
       <:expr<Nvbindlib.mbind $fv$
               (fun $lid:id$ -> $e$) >>
     ] ];

  expr: LEVEL "simple" 
    [ [
    "[^"; "^]" -> <:expr< Nvbindlib.unit [] >>      
  | "[^"; el = expr1_semi_list; "^]" -> <:expr< $mklistupexp _loc true el$ >>      
  | "[|^"; "^|]" -> <:expr< Nvbindlib.unit [||] >>
  | "[|^"; el = expr1_semi_list; "^|]" -> 
      mkarrayupexp _loc el
  | "{^"; lel = label_expr; "^}" ->
      mkrecordupexp _loc None lel
  | "{^"; e = expr LEVEL "."; "with"; le = label_expr; "^}" ->
      mkrecordupexp _loc (Some e) le
 	| "(^"; "^)" -> <:expr< Nvbindlib.unit () >>
	| "(^"; e = expr LEVEL ":="; "^)" -> <:expr< Nvbindlib.unit $e$ >>
  | "(^"; e = expr LEVEL ":="; ","; el = LIST1 expr LEVEL ":=" SEP ","; "^)" -> 
      mktuppleupexp _loc (e::el)
    ] ];

  expr: AFTER "^" 
    [ 
      RIGHTA 
	[ e1 = SELF; "^::"; e2 = SELF -> 
          <:expr< Nvbindlib.unit_apply2 (fun x y -> [ x::y ]) $e1$ $e2$ >> ]
    ];

  expr1_semi_list:
    [ [ e = expr LEVEL "expr1"; ";"; el = SELF -> e :: el
      | e = expr LEVEL "expr1"; ";" -> [e]
      | e = expr LEVEL "expr1" -> [e] ] ]
  ;

  vbinding:
    [ [
      "as"; e = expr LEVEL "apply" -> Some e
  | ->
      None ] ]
    ;
 
  freshin:
    [ [
      "for"; id = LIDENT -> Some id
  | ->
      None ] ]
    ;
		
  lbl_expr_list:
    [ [ le = lbl_expr; ";"; lel = SELF -> le :: lel
      | le = lbl_expr; ";" -> [ le ]
      | le = lbl_expr -> [ le ] ] ]
  ;
  lbl_expr:
    [ [ i = ident; "="; e = expr LEVEL "expr1" -> (i, e) ] ]
  ;
  END	
;;


end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Extension)
