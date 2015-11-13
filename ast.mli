type expr =
  | StrLiteral of string
  | Var of string
  | Assign of string * expr

type stmt = 
  | Block of stmt list
  | Expr of expr

type func_decl = {
  fname : string;         (* Name of the function *)
  formals : string list;  (* Formal argument names *)
  locals : string list;   (* Locally defined variables *)
  body : stmt list;
}

type program = string list * func_decl list
