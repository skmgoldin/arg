type expr =
  | StrLiteral of string
  | Id of string
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type program = expr list
