type t =
  | Nil
  | Ldc of int
  | Ld of int * int
  | Ap
  | Rap
  | Dum
  | Sel
  | Join
  | Ldf of t list
  | Sel of t list * t list

let index vname names =
  let rec indx names i = match names with
    | [] -> failwith "Variable " ^ vname ^ " not found in environment"
    | xs :: xss ->
      (let rec indx2 xs j = match xs with
         | [] -> indx xss (i+1)
         | n :: xss' when n = vname -> (i, j)
         | _ :: xss' -> indx2 xss' (j + 1)
       in
       indx2 xs 0)
  in
  indx names 0

let rec compile exp names acc = match exp with
  | Exp.Nil -> Nil :: acc
  | Exp.Int n -> Ldc n :: acc
  | Exp.Var s -> Ld (index s names) :: acc
  | Exp.If (cond, tcase, fcase) -> compile_if cond tcase fcase names acc
  | Exp.Lambda (params, body) -> compile_lambda body (params :: names) acc
  | Exp.Let (vars, exps, body) ->
      let cbody = compile_lambda body (vars :: names) (Ap :: acc) in
      Nil :: compile_app exps names cbody
  | Exp.LetRec (vars, exps, body) ->
      let names' = vars :: names in
      let cbody = compile_lambda body names' (Rap :: acc) in
      Dum :: Nil :: compile_app exps names' cbody
  | Exp.Call (Exp.Var fname, args) when List.mem fname builtins ->
      compile_builtin fname args names acc
  | Exp.Call (Exp.Var fname, args) ->
      let cfunc = Ld (index fname names) :: Ap :: acc in
      Nil :: compile_app args names cfunc
  | Exp.Call (exp1, args) ->
      let cfunc = compile exp1 names (Ap :: acc) in
      Nil :: compile_app args names cfunc
  and compile_if cond tcase fcase names acc =
    let ctcase = compile tcase names [Join] in
    let cfcase = compile fcase names [Join] in
    let acc' = Sel (ctcase, cfcase) :: acc in
    compile cond names acc'
  and compile_lambda body names acc =
    Ldf (compile body names [Rtn]) :: acc
