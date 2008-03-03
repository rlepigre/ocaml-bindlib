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
  | [] -> <:expr@loc< Bindlib.unit [] >>
  | e1::el -> 
      <:expr@loc< Bindlib.unit_apply2 (fun x y -> [ x::y ]) $e1$ $mklistupexp loc false el$ >>  

let mktuppleupexp loc el =
  match el with 
    [] ->
      <:expr@loc< Bindlib.unit () >>
  | [e] ->
      <:expr@loc< Bindlib.unit $e$ >>
  | _ ->
      let et, preambule, preambule2, _ =
	List.fold_left 
	  (fun (et,ex,el,i) e ->
	    let namex = "x#"^string_of_int i in
	    let namey = "y#"^string_of_int i in
	    (match et with
	      Some et -> Some <:expr@loc<$et$, $lid:namex$ $lid:"v#"$>>
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
let vbinding = Gram.Entry.mk "bindlib_binding"
let freshin = Gram.Entry.mk "bindlib_freshin"
let lbl_expr_list = Gram.Entry.mk "bindlib_lbl_expr_list"
let lbl_expr = Gram.Entry.mk "bindlib_lbl_expr"

let _ =
EXTEND Gram
  expr: LEVEL "apply"
    [ [ e1 = SELF; "^^"; e2 = SELF ->
	<:expr<Bindlib.bind_apply $e1$ $e2$>>
    | e1 = SELF; "^|^"; e2 = SELF ->
	<:expr<Bindlib.mbind_apply $e1$ $e2$>>
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
	  [] -> <:expr<Bindlib.unit $!e$>> 
	| [e1] ->  <:expr<Bindlib.unit_apply $!e$ $e1$ >>
	| [e1;e2] -> <:expr<Bindlib.unit_apply2 $!e$ $e1$ $e2$ >>
	| e1::e2::el ->
	    let e0 = <:expr<Bindlib.unit_apply2 $!e$ $e1$ $e2$ >> in
	    List.fold_left (fun e1 e2 -> <:expr<Bindlib.apply $e1$ $e2$ >>) 
	      e0 el
      end else begin
	<:expr<$e1$ $mktuppleupexp _loc el$>>
      end ] ]
    ;
  
  expr: LEVEL "top"
    [ [
    "letvar"; fv = expr LEVEL "simple"; id = LIDENT; str = vbinding;
      fr = freshin;
      "in";  x = sequence ->
	let name = match str with
	  Some e -> e
	| None ->  <:expr< $str:id$ >> 
	in
	begin match fr with
	  None ->
	    <:expr<let $lid:id$ = Bindlib.new_var $fv$ $name$ 
            in do {$seq:x$}>>
	| Some ctxt ->
	    <:expr<let ($lid:id$,$lid:ctxt$) = 
	      Bindlib.new_var_in $lid:ctxt$ $fv$ $name$ 
            in do{$seq:x$}>>
	end
  | "letvar"; fv = expr LEVEL "simple"; id = LIDENT; "("; n = expr LEVEL "top";")"; 
	str = vbinding; fr = freshin; "in";  x = sequence ->
	let names = match str with
	  Some e -> e
	| None -> 
	    <:expr<Array.init $n$ (fun i -> $str:id$^(string_of_int i))>>
	in
	begin match fr with
	  None ->
	    <:expr<let $lid:id$ = Bindlib.new_mvar $fv$ $names$ 
            in do{$seq:x$}>>
	| Some ctxt ->
	    <:expr<let ($lid:id$,$lid:ctxt$) = 
	      Bindlib.new_mvar_in $lid:ctxt$ $fv$ $names$ 
            in do{$seq:x$}>>
	end
     ] ];

  match_case0: 
    [ [
    "bind"; fv = expr LEVEL "simple";  
      id = LIDENT; str = vbinding;  fr = freshin; "in"; g = LIDENT;
      "->"; f = sequence -> 
	let e1 = 
	  <:expr<let $lid:g$ = Bindlib.subst $lid:"#e"$ (Bindlib.free_of $lid:id$) in do{$seq:f$}>> 
	in
	let name = match str with
	  None -> <:expr<Bindlib.binder_name $lid:"#e"$>>
	| Some name -> name
	in
	let e2 = match fr with
	  None -> 
	    <:expr<let $lid:id$ = Bindlib.new_var $fv$ $name$ in $e1$>> 
	| Some ctxt -> 
	    <:expr<let ($lid:id$,$lid:ctxt$) = 
	      Bindlib.new_var_in $lid:ctxt$ $fv$ $name$ in $e1$>> 
	in
	<:match_case<
        $lid:"#e"$ -> $e2$>>
  | "bind"; fv = expr LEVEL "simple";  
      id = LIDENT; "("; arity = LIDENT; ")"; 
	str = vbinding;  fr = freshin; "in"; g = LIDENT;
      "->"; f = sequence -> 
	let e1 = 
	  <:expr<let $lid:g$ = Bindlib.msubst $lid:"#e"$ (Array.map Bindlib.free_of $lid:id$) in do{$seq:f$}>> 
	in
	let names = match str with
	  None -> 
            <:expr<Bindlib.mbinder_names $lid:"#e"$>>
	| Some names -> 
	    <:expr<do {if (Array.length $names$ <> $lid:arity$) then
	      invalid_argument "bad array size for names array in match ... with bind"
		else (); 
	    $names$}>>
	in
	let e2 = match fr with
	  None -> 
	    <:expr<
	    let $lid:id$ = Bindlib.new_mvar $fv$ $names$ in $e1$>> 
	| Some ctxt -> 
	    <:expr<
	    let ($lid:id$,$lid:ctxt$) = 
	      Bindlib.new_mvar_in $lid:ctxt$ $fv$ $names$ in $e1$>> 
	in
	<:match_case<
	$lid:"#e"$ ->
	let $lid:arity$ = Bindlib.mbinder_arity $lid:"#e"$ in
	$e2$
	>>
     ] ];

  expr: LEVEL "~-"
    [ [
    "bindvar"; id = LIDENT; "in"; e = expr LEVEL "top" -> 
      <:expr<Bindlib.bind_var $lid:id$ $e$>>
  | "bindvar"; id = LIDENT; "("; ")"; "in"; e = expr LEVEL "top" -> 
      <:expr<Bindlib.bind_mvar $lid:id$ $e$>>
  | "bind"; fv = expr LEVEL "simple"; id = LIDENT; str = vbinding; 
      fr = freshin; "in"; e = expr LEVEL "top" -> 
	   let name = match str with
	     Some e -> e
	   | None ->  <:expr< $str:id$ >> 
	   in
	   begin match fr with
	     None ->
	       <:expr<Bindlib.bind $fv$ $name$ 
               (fun $lid:id$ -> $e$) >>
	   | Some ctxt -> 
	       <:expr<Bindlib.bind_in $lid:ctxt$ $fv$ $name$ 
               (fun $lid:id$ $lid:ctxt$ -> $e$) >>
	   end
  | "bind"; fv = expr LEVEL "simple"; id = LIDENT; "("; n = expr LEVEL "top"; ")"; 
	 str = vbinding;  fr = freshin; "in"; e = expr LEVEL "top" -> 
	   let names = match str with
	     Some e -> e
	   | None -> 
	       <:expr<Array.init $n$ (fun i -> $str:id$^(string_of_int i))>>
	   in
	   begin match fr with
	     None ->
	       <:expr<Bindlib.mbind $fv$ $names$ 
               (fun $lid:id$ -> $e$) >>
	   | Some ctxt ->
	       <:expr<Bindlib.mbind_in $lid:ctxt$ $fv$ $names$ 
               (fun $lid:id$ $lid:ctxt$ -> $e$) >>
	   end
     ] ];

  expr: LEVEL "simple" 
    [ [
    "[^"; "^]" -> <:expr< Bindlib.unit [] >>      
  | "[^"; el = expr1_semi_list; "^]" -> <:expr< $mklistupexp _loc true el$ >>      
  | "[|^"; "^|]" -> <:expr< Bindlib.unit [||] >>
  | "[|^"; el = expr1_semi_list; "^|]" -> 
      mkarrayupexp _loc el
  | "{^"; lel = label_expr; "^}" ->
      mkrecordupexp _loc None lel
  | "{^"; e = expr LEVEL "."; "with"; le = label_expr; "^}" ->
      mkrecordupexp _loc (Some e) le
	| "(^"; "^)" -> <:expr< Bindlib.unit () >>
	| "(^"; e = expr LEVEL ":="; "^)" -> <:expr< Bindlib.unit $e$ >>
  | "(^"; e = expr LEVEL ":="; ","; el = LIST1 expr LEVEL ":=" SEP ","; "^)" -> 
      mktuppleupexp _loc (e::el)
    ] ];

  expr: AFTER "^" 
    [ 
      RIGHTA 
	[ e1 = SELF; "^::"; e2 = SELF -> 
          <:expr< Bindlib.unit_apply2 (fun x y -> [ x::y ]) $e1$ $e2$ >> ]
    ];

  expr1_semi_list:
    [ [ e = expr LEVEL "top"; ";"; el = SELF -> e :: el
      | e = expr LEVEL "top"; ";" -> [e]
      | e = expr LEVEL "top" -> [e] ] ]
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
