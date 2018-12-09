type ident =
  | Ident of string

type t =
  | Nil
  | Int of int
  | Var of ident
  | If of t * t * t
  | Lambda of ident list * t
  | Let of ident list * t list * t
  | LetRec of ident list * t list * t
  | Call of t list
