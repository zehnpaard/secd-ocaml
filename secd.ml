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

let car i = match cells.(i) with
  | Cons (i, _) -> i
  | _ -> failwith "Taking car of non-cons cell"

let cdr i = match cells.(i) with
  | Cons (_, j) -> j
  | _ -> failwith "Taking cdr of non-cons cell"

let popR r = match cells.(!r) with
  | Cons (i, j) -> (r := j; i)
  | _ -> failwith "Register points to a non-cons cell"

let pushR r i =
  r := makeCons i !r

let runOneStep () = match cells.(popR c) with
  | Int 0 (* STOP *) -> c := 0
  | Int 1 (* NIL *) -> pushR s 0
  | Int 2 (* LDC *) -> pushR s (popR c)
  | Int 3 (* LD *) -> e
  | Int _ -> failwith "Unknown command"
  | _ -> failwith "Cons cell found in place of command"

let rec run () =
  if !c = 0 then ()
  else (runOneStep (); run ())
