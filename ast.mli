type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
  | Assign of string * expr
  | Call of string * expr list
  | Id of string
  | StrLiteral of string
  | Binop of expr * op * expr
  | Noexpr

type statement =
  | Expr of expr
  | Loop of loop

type loop =


type function = string * Id list * statement list

type program = expr list
