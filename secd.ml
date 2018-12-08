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

let getn i = match cells.(i) with
  | Int n -> n
  | _ -> failwith "Getting number from cons cell"

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

let rec locate i j =
  let rec locatej j env =
    if j = 0 then car env
    else locatej (j - 1) (cdr env)
  in
  let rec locatei i j env =
    if i = 0 then locatej j (car env)
    else locatei (i - 1) j (cdr env)
  in
  locatei i j !e

let binOp r op =
  let a = popR r in
  let b = popR r in
  pushR r (op a b)

let runOneStep () = match cells.(popR c) with
  | Int 0 (* STOP *) -> c := 0
  | Int 1 (* NIL *) -> pushR s 0
  | Int 2 (* LDC *) -> pushR s (popR c)
  | Int 3 (* LD *) ->
      let ij = popR c in
      let i = getn (car ij) in
      let j = getn (cdr ij) in
      pushR s (locate i j)
  | Int 4 (* CAR *) -> pushR s (car (popR s))
  | Int 5 (* CDR *) -> pushR s (cdr (popR s))
  | Int 6 (* ATOM *) ->
      (match cells.(popR s) with
         | Int _ -> pushR s (makeInt 1)
         | _ -> pushR s (makeInt 0))
  | Int 7 (* CONS *) -> binOp s (fun car_, cdr_ -> makeCons car_ cdr_)
  | Int 8 (* EQ *) -> binOp s (fun a, b -> makeInt (if a = b then 1 else 0))
  | Int 9 (* LEQ *) -> binOp s (fun a, b -> makeInt (if a <= b then 1 else 0))
  | Int 10 (* ADD *) -> binOp s (fun a, b -> makeInt (a + b))
  | Int 11 (* SUB *) -> binOp s (fun a, b -> makeInt (a - b))
  | Int 12 (* MUL *) -> binOp s (fun a, b -> makeInt (a * b))
  | Int 13 (* DIV *) -> binOp s (fun a, b -> makeInt (a / b))
  | Int 14 (* REM *) -> binOp s (fun a, b -> makeInt (a mod b))
  | Int _ -> failwith "Unknown command"
  | _ -> failwith "Cons cell found in place of command"

let rec run () =
  if !c = 0 then ()
  else (runOneStep (); run ())
