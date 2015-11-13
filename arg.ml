open Ast

(* TODO: Mapping of vdecls to values *)
let rec eval = function
	| Binop(e1, op, e2) ->
		let v1 = eval e1 and v2 = eval e2 in
		match op with
			Add -> v1 + v2
			| Sub -> v1 - v2
			| Mult -> v1 * v2
			| Div -> v1 / v2

(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
*)

let rec string_of_stmt = function
  | Expr(expr) -> string_of_expr expr ^ ";\n";

let rec string_of_expr = function
  | StrLiteral(l) -> l
  | Id(s) -> s
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Noexpr -> ""



let rec translateProgram = function
	| [] -> ""
	| stmnt :: tl -> (string_of_stmnt stmnt) ^ translateProgram tl

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  execute program
