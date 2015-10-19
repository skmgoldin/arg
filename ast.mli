type operator = Add | Sub | Mul | Div | Cmp

type expr =
	Binop of expr * operator * expr
	| Lit of int
	| Seq of expr * expr
	| Asn of int * expr
	| Var of int
	| Cmp of expr * expr
