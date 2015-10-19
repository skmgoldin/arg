open Ast

let vars = Array.make 26 0

let addtovars var a =
	vars.(var) <- a; a

let rec eval = function
	Lit(x) -> x
	| Var(x) -> vars.(x)
	| Asn(x, e1) -> addtovars x (eval e1)
	| Seq(x1, x2) -> (eval x1) + (eval x2)
	| Cmp(x1, x2) -> Asn(x1, x2)
	| Binop(e1, op, e2) ->
		let v1 = eval e1 and v2 = eval e2 in
		match op with
			Add -> v1 + v2
			| Sub -> v1 - v2
			| Mul -> v1 * v2
			| Div -> v1 / v2

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
