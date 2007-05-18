let Idt = /\X:*. X -> X : *;
let idt = \X:*. \x:X. x : Idt;

let Nat = /\X:*. (X -> X) -> X -> X : *;

let zero = \X:*. \f:X->X. \a:X. a : Nat;
let succ = \n:Nat. \X:*. \f:X->X. \a:X. f (n X f a) : Nat -> Nat;
let add = \n,p:Nat. \X:*. \f:X->X. \a:X. n X f (p X f a) : Nat -> Nat -> Nat;
let mul = \n,p:Nat. \X:*. \f:X->X. \a:X. n X (p X f) a : Nat -> Nat -> Nat;
let exp = \n,p:Nat. \X:*. p (X -> X) (n X) : Nat -> Nat -> Nat;

let pred = \n:Nat. n (Nat -> Nat -> Nat)
      (\f:Nat -> Nat -> Nat. \x,y:Nat. f (succ x) x)
      (\x,y:Nat. y)
      zero
      zero 
  : Nat -> Nat;

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

