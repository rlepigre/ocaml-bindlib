
let fold_map f tbl acc =
  let acc = ref acc in
  let fn x = 
    let x', acc' = f x !acc in
    acc := acc';
    x'
  in
  let tbl' = Array.map fn tbl in
  tbl', !acc
