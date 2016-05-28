let Idt = ∀X:*. X → X : *;
// test
let idt = λX:*. λx:X. x : Idt;

let Nat = ∀X:*. (X → X) → X → X : *;

let zero = λX:*. λf:X→X. λa:X. a : Nat;
let succ = λn:Nat. λX:*. λf:X→X. λa:X. f (n X f a) : Nat → Nat;
let add = λn,p:Nat. λX:*. λf:X→X. λa:X. n X f (p X f a) : Nat → Nat → Nat;
let mul = λn,p:Nat. λX:*. λf:X→X. λa:X. n X (p X f) a : Nat → Nat → Nat;
let exp = λn,p:Nat. λX:*. p (X → X) (n X) : Nat → Nat → Nat;

let pred = λn:Nat. n (Nat → Nat → Nat)
      (λf:Nat → Nat → Nat. λx,y:Nat. f (succ x) x)
      (λx,y:Nat. y)
      zero
      zero
  : Nat → Nat;

let n0 = zero : Nat;
let n1 = succ n0 : Nat;
let n2 = succ n1 : Nat;
let n3 = succ n2 : Nat;
let n4 = succ n3 : Nat;
let n5 = succ n4 : Nat;
let n6 = succ n5 : Nat;
let n7 = succ n6 : Nat;
let n8 = succ n7 : Nat;
let n9 = succ n8 : Nat;
let n10 = succ n9 : Nat;

n10 : Nat;

add n10 n10 : Nat;

mul n10 n10 : Nat;

exp n4 n3 : Nat;

pred n10 : Nat;

let p = Nat × Nat × (Nat → Nat): *;

let x = [n3, n4, succ] : p;

let y1 = x.1 : Nat;
let y2 = x.2.1 : Nat;
let y3 = x.2.2 : Nat → Nat;
let y4 = y3 n5 : Nat;
