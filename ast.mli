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
  | If of expr * statement list
  | While of expr * statement list

type func = {
  fname : string;
  formals : string list;
  body : statement list;
}

type program = func list * statement list
