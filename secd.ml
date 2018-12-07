type t =
  | Int of int
  | Cons of int * int

let cells = Array.make 1000 (Int 0)

let s = ref 0;
let e = ref 0;
let c = ref 0;
let d = ref 0;
let f = ref 1;

let makeInt i =
  let n = !f in
  (cells.(n) <- Int i; f := n + 1; n)

let makeCons i j =
  let n = !f in
  (cells.(n) <- Cons (i, j); f := n + 1; n)

let rec f () =
  if !c = 0 then ()
  else (g (); f ())
