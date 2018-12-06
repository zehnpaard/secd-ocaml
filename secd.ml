type t =
  | Int of int
  | Cons of int * int

let cells = Array.make 1000 (Int 0)
