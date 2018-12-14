type t =
  | Int of int
  | Cons of int * int

let cells = Array.make 100000 (Int 0)

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

let rplaca x y =
  (cells.(x) := Cons (y, cdr x); x)

let load_simple_command i n =
  makeCons (makeInt n) i

let rec load_commands' i = function
  | [] -> i
  | x :: xs ->
      load_commands' (load_command i x) xs
and load_command i = function
  | Compile.Stop -> load_simple_command i 0
  | Compile.Nil -> load_simple_command i 1
  | Compile.Ldc n ->
      let j = makeCons (makeInt n) i in
      load_simple_command 2 j
  | Compile.Ld (n, m) ->
      let j = makeCons (makeInt n) (makeInt m) in
      load_simple_command 3 j
  | Compile.Car -> load_simple_command i 4
  | Compile.Cdr -> load_simple_command i 5
  | Compile.Atom -> load_simple_command i 6
  | Compile.Cons -> load_simple_command i 7
  | Compile.Eq -> load_simple_command i 8
  | Compile.Leq -> load_simple_command i 9
  | Compile.Add -> load_simple_command i 10
  | Compile.Sub -> load_simple_command i 11
  | Compile.Mul -> load_simple_command i 12
  | Compile.Div -> load_simple_command i 13
  | Compile.Rem -> load_simple_command i 14
  | Compile.Sel (ts, fs) ->
      let j = load_commands' i (List.rev fs) in
      let k = load_commands' j (List.rev ts) in
      load_simple_command 15 k
  | Compile.Join -> load_simple_command i 16
  | Compile.Ldf cs ->
      let j = load_commands' i (List.rev cs) in
      load_simple_command 17 j
  | Compile.Ap -> load_simple_command i 18
  | Compile.Rtn -> load_simple_command i 19
  | Compile.Dum -> load_simple_command i 20
  | Compile.Rap -> load_simple_command i 21

let load_commands commands =
  c := load_commands' (List.rev commands)

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
  | Int 8 (* EQ *) -> binOp s (fun a, b -> makeInt (if getn a = getn b then 1 else 0))
  | Int 9 (* LEQ *) -> binOp s (fun a, b -> makeInt (if getn a <= getn b then 1 else 0))
  | Int 10 (* ADD *) -> binOp s (fun a, b -> makeInt (getn a + getn b))
  | Int 11 (* SUB *) -> binOp s (fun a, b -> makeInt (getn a - getn b))
  | Int 12 (* MUL *) -> binOp s (fun a, b -> makeInt (getn a * getn b))
  | Int 13 (* DIV *) -> binOp s (fun a, b -> makeInt (getn a / getn b))
  | Int 14 (* REM *) -> binOp s (fun a, b -> makeInt (getn a mod getn b))
  | Int 15 (* SEL *) ->
      let cond = match cells.(popR s) with
        | Int 0 -> false
        | _ -> true
      in
      let true_branch = popR c in
      let false_branch = popR c in
      (pushR d !c; c := if cond then true_branch else false_branch)
  | Int 16 (* JOIN *) -> c := popR d
  | Int 17 (* LDF *) ->
      let func = popR c in
      pushR s (makeCons func !e)
  | Int 18 (* AP *) ->
    (* (f.e' a).s e AP.c d -> nil a.e' f (s e c).d *)
      let fe = popR s in
      let func = car fe in
      let env = cdr fe in
      let arg = popR s in
      begin
        pushR d !c;
        c := func;
        pushR d !e;
        e := makeCons arg env;
        pushR d !s;
        s := 0;
      end; ()
  | Int 19 (* RTN *) ->
      let retv = popR s in
      begin
        s := makeCons retv (popR d);
        e := (popR d);
        c := (popR d);
      end; ()
  | Int 20 (* DUM *) -> pushR e 0
  | Int 21 (* RAP *) ->
      (* ((f.(nil.e)) v.s) (nil.e) (RAP.c) d ->
         nil rplaca((nil.e),v) f (s e c.d) *)
      (* (rplaca((nil.e),v).e) according to AoSC ?? *)
      let fe = popR s in
      let func = car fe in
      let nenv = cdr fe in
      let arg = popR s in
      begin
        pushR d !c;
        c := func;
        pushR d (cdr !e);
        e := rplaca nenv arg;
        pushR d !s;
        s := 0;
      end; ()
  | Int _ -> failwith "Unknown command"
  | _ -> failwith "Cons cell found in place of command"

let rec run () =
  if !c = 0 then ()
  else (runOneStep (); run ())
