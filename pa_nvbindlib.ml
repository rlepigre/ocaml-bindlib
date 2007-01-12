open MLast
open Pcaml 
 
let rec mklistupexp _loc top = function
  | [] -> <:expr< Nvbindlib.unit [] >>
  | e1::el -> 
      let _loc = if top then _loc else (fst (MLast.loc_of_expr e1), snd _loc) in
      <:expr< Nvbindlib.unit_apply2 (fun x y -> [ x::y ]) $e1$ $mklistupexp _loc false el$ >>  

let mktuppleupexp _loc el =
  match el with 
    [] ->
      <:expr< Nvbindlib.unit () >>
  | [e] ->
      <:expr< Nvbindlib.unit $e$ >>
  | _ ->
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
      let right = preambule2 <:expr<(fun $lid:"v#"$ -> ($list:List.rev et$))>>
      in
      preambule <:expr<special_end $lid:"e#"$ (fun $lid:"h#"$ -> $right$)>>

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
  let et, preambule, _ =
    List.fold_left 
      (fun (et,ex,i) (lbl,e) ->
	let name = "x#"^string_of_int i in
	(lbl, <:expr<$lid:name$ $lid:"h#"$ $lid:"v#"$>>)::et,
        (fun next -> 
	  ex (<:expr<let ($lid:"e#"$, $lid:name$) = special_apply $lid:"e#"$ $e$ in $next$>>)),
	i+1)
      ([], start, 0)
      le
  in 
  preambule <:expr<special_end $ExLid(_loc,"e#")$
		       (fun $lid:"h#"$ $lid:"v#"$ -> $ExRec(_loc,List.rev et,ew)$)>>

let expr_ident = Grammar.Entry.create gram "bindlib_expr_ident"
let expr1_semi_list = Grammar.Entry.create gram "bindlib_expr1_semi_list"
let lbl_expr_list = Grammar.Entry.create Pcaml.gram "bindlib_lbl_expr_list"
let lbl_expr = Grammar.Entry.create Pcaml.gram "bindlib_lbl_expr"
let patt_label_ident = Grammar.Entry.create Pcaml.gram "bindlib_patt_label_ident"

let _ =
  EXTEND 
  expr: LEVEL "apply"
    [ [
      e1 = SELF; "^^"; e2 = SELF ->
	<:expr<Nvbindlib.bind_apply $e1$ $e2$>>
    | e1 = SELF; "^|^"; e2 = SELF ->
	<:expr<Nvbindlib.mbind_apply $e1$ $e2$>>
    | e1 = SELF; "(^"; el = LIST0 expr LEVEL "^" SEP "," ;"^)" ->
      if Pa_o.is_expr_constr_call e1 then begin
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
  
  expr: LEVEL "expr1"
    [ [
    "letvar"; fv = expr LEVEL "simple"; id = LIDENT;
      "in";  x = expr ->
	<:expr<let $lid:id$ = Nvbindlib.new_var $fv$ in $x$>>
  | "letvar"; fv = expr LEVEL "simple"; id = LIDENT; "("; n = expr LEVEL "top";")"; x = expr ->
      <:expr<let $lid:id$ = Nvbindlib.new_mvar $fv$ $n$ in $x$>>
  | "match"; e = SELF; "with"; "bind"; fv = expr LEVEL "simple";  
      id = LIDENT; "in"; g = LIDENT;
      "->"; f = expr LEVEL "top" -> 
	let e1 = 
	  <:expr<let $lid:g$ = Nvbindlib.subst $e$ (Nvbindlib.free_of $lid:id$) in $f$>> 
	in
	<:expr<let $lid:id$ = Nvbindlib.new_var $fv$ in $e1$>> 
  | "match"; e = SELF; "with"; "bind"; fv = expr LEVEL "simple";  
      id = LIDENT; "("; arity = LIDENT; ")"; "in"; g = LIDENT;
      "->"; f = expr LEVEL "top" -> 
	let e1 = 
	  <:expr<let $lid:g$ = Nvbindlib.subst $lid:"#e"$ (Nvbindlib.free_of $lid:id$) in $f$>> 
	in
	let e2 =
	  <:expr<
	  let $lid:id$ = Nvbindlib.new_mvar $fv$ $lid:arity$ in $e1$>> 
	in
	<:expr<
	let $lid:"#e"$ = $e$ in
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
  | "bind"; fv = expr LEVEL "simple"; id = LIDENT; "in"; e = expr LEVEL "top" -> 
      <:expr<Nvbindlib.bind $fv$ (fun $lid:id$ -> $e$) >>
  | "bind"; fv = expr LEVEL "simple"; id = LIDENT; "("; n = expr LEVEL "top"; ")"; 
        "in"; e = expr LEVEL "top" -> 
	  <:expr<Nvbindlib.mbind $fv$ $n$
          (fun $lid:id$ -> $e$) >>
     ] ];

  expr: LEVEL "simple" 
    [ [
    "[^"; "^]" -> <:expr< Nvbindlib.unit [] >>      
  | "[^"; el = expr1_semi_list; "^]" -> <:expr< $mklistupexp _loc true el$ >>      
  | "[|^"; "^|]" -> <:expr< Nvbindlib.unit [||] >>
  | "[|^"; el = expr1_semi_list; "^|]" -> 
      mkarrayupexp _loc el
  | "{^"; Pa_o.test_label_eq; le = lbl_expr_list; "^}" ->
      mkrecordupexp _loc None le
  | "{^"; e = expr LEVEL "."; "with"; le = lbl_expr_list; "^}" ->
      mkrecordupexp _loc (Some e) le
  | "(^"; "^)" -> <:expr< Nvbindlib.unit () >>
  | "(^"; el =  LIST1 expr LEVEL ":=" SEP ","; "^)" -> 
      mktuppleupexp _loc el
    ] ];

  expr: AFTER "^" 
    [ 
      RIGHTA 
	[ e1 = SELF; "^::"; e2 = SELF -> 
          <:expr< Nvbindlib.unit_apply2 (fun x y -> [ x::y ]) $e1$ $e2$ >> ]
    ];

  expr_ident:
    [ RIGHTA
      [ i = LIDENT -> <:expr< $lid:i$ >>
      | i = UIDENT -> <:expr< $uid:i$ >>
      | i = UIDENT; "."; j = SELF ->
          let rec loop m =
            function
             <:expr< $x$ . $y$ >> -> loop <:expr< $m$ . $x$ >> y
            | e -> <:expr< $m$ . $e$ >> 
          in
          loop <:expr< $uid:i$ >> j
      | i = UIDENT; "."; "("; j = Pa_o.operator_rparen ->
          <:expr< $uid:i$ . $lid:j$ >> ] ]
  ;

  expr1_semi_list:
    [ [ e = expr LEVEL "expr1"; ";"; el = SELF -> e :: el
      | e = expr LEVEL "expr1"; ";" -> [e]
      | e = expr LEVEL "expr1" -> [e] ] ]
  ;

  lbl_expr_list:
    [ [ le = lbl_expr; ";"; lel = SELF -> le :: lel
      | le = lbl_expr; ";" -> [ le ]
      | le = lbl_expr -> [ le ] ] ]
  ;
  lbl_expr:
    [ [ i = patt_label_ident; "="; e = expr LEVEL "expr1" -> (i, e) ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | RIGHTA
      [ i = UIDENT -> <:patt< $uid:i$ >>
      | i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;
  END	
;;
