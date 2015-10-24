type op = Add | Sub | Mult | Div | Eq | Neq | Lt | Gt | Leq | Geq 

type expr =
  | IntLiteral of int  
  | FloatLiteral of float  
  | StrLiteral of string  
  | BoolLiteral of bool
  | Noexpr
  | Var of string
  | Assign of string * expr
  | Binop of expr * op * expr
  | Call of string * expr list 

type stmt = 
  | Block of stmt list
  | Expr of expr
  | Ret of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Stop of char list

type func_decl = {
  fname : string;         (* Name of the function *)
  formals : string list;  (* Formal argument names *)
  locals : string list;   (* Locally defined variables *)
  body : stmt list;
}

type program = string list * func_decl list
