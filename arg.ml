open Ast

let rec eval = function

let rec eval = function
	| IntLit(x) -> x
	| StrLit(x) -> x
	| Seq(x1, x2) -> (eval x1) + (eval x2)
	| Asn(x, e1) -> let x = (eval e1)
	| Cmp(e1, e2) -> (eval e1) = (eval e2)
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
