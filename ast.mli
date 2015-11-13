type expr =
  | StrLiteral of string
  | Id of string
  | Assign of string * expr

type program = expr list
