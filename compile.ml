open Exp

let rec compile exp names acc = match exp with
  | Nil -> C.nil :: acc
  | Int n -> C.ldc :: n :: acc
  | Var s -> C.ld :: index s names :: acc
  | If (cond, tcase, fcase) -> compile_if cond tcase fcase names acc
  | Lambda (params, body) -> compile_lambda body (params :: names) acc
  | Let (vars, exps, body) ->
      let cbody = compile_lambda body (vars :: names) (C.ap :: acc) in
      C.nil :: compile_app exps names cbody
  | LetRec (vars, exps, body) ->
      let names' = vars :: names in
      let cbody = compile_lambda body names' (C.rap :: acc) in
      C.dum :: C.nil :: compile_app exps names' cbody
  | Call (Var fname, args) when List.mem fname builtins ->
      compile_builtin fname args names acc
  | Call (Var fname, args) ->
      let cfunc = C.ld :: index fname names :: C.ap :: acc in
      C.nil :: compile_app args names cfunc
  | Call (exp1, args) ->
      let cfunc = compile exp1 names (C.ap :: acc) in
      C.nil :: compile_app args names cfunc
