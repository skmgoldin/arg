type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
  | Assign of string * expr
  | Call of string * expr list
  | Id of string
  | ArrId of string * expr
  | StrLiteral of string
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool  
  | Binop of expr * op * expr

type statement =
  | Expr of expr
  | If of expr * statement list
  | IfElse of expr * statement list * statement list
  | While of expr * statement list
  | ArrayAssign of string * expr * expr list
  | ArrayElemAssign of string * expr * expr
  | Print of string * expr
  | Return of expr

type func = {
  fname : string;
  formals : string list;
  body : statement list;
}

type program = func list * statement list
