Semantics of Bindlib (sketch / work in progress)
================================================

### Variables

A variable `x` is represented as a structure with the following fields:
- `x.uid` containing a unique identifier,
- `x.name` containing a prefered name,
- `x.mkfree` a function injectiong the variable in the type of the variable.

### Unboxing and environment

The `unbox` function is specified using an environment `e` (internal to the
implementation), which is an association list from variable unique identifiers
to values. We will write `unbox{e}(v)` to make the environment explicit.

### Equational semantics

```
                        unbox(v) = unbox{[]}(v)
                 unbox{e}(box v) = v
         unbox{e}(apply_box f v) = (unbox{e}(f)) (unbox{e}(v))
             unbox{e}(box_var x) = try List.assoc x.uid e with Not_found -> x.mkfree x
subst (unbox{e}(bind_var x f)) v = unbox{(x.uid,v)::e}(f)
        unbox{e}(box_pair v1 v2) = (unbox{e}(v1), unbox{e}(v2))
           unbox{e}(box_list vs) = List.map (fun v -> unbox{e}(v)) vs

```
