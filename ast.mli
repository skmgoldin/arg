type op = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Leq | Geq 

type expr =
  | Literal of int  
  | Noexpr

type stmt = 
  | Block of stmt list

type func_decl = {
  fname : string;         (* Name of the function *)
  formals : string list;  (* Formal argument names *)
  locals : string list;   (* Locally defined variables *)
  body : stmt list;
}

type program = string list * func_decl list
