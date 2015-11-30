type expr =
  | Assign of string * expr
  | Call of string * expr list
  | Id of string
  | StrLiteral of string
  | Noexpr

type statement =
  | Expr of expr
  | Loop of loop


type function = Function of string * Id list * statement list

type program = expr list
