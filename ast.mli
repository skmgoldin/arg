type expr =
  | Assign of string * expr
  | Call of string * expr list
  | Id of string
  | Noexpr
  | StrLiteral of string

type program = expr list
