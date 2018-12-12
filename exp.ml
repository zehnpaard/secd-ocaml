type t =
  | Nil
  | Int of int
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | Let of string list * t list * t
  | LetRec of string list * t list * t
  | Call of t * t list
