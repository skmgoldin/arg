type operator = Add | Sub | Mul | Div | Cmp

type expr =
	Binop of expr * operator * expr
	| IntLit of int
	| StrLit of string 
	| Seq of expr * expr
	| Asn of int * expr
	| Var of int
	| Cmp of expr * expr
