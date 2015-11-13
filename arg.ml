open Ast



(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
*)

let rec string_of_expr = function
  | StrLiteral(l) -> l
  | Id(s) -> s
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Noexpr -> ""
	| _ -> "fuck off"

let rec string_of_stmnt expr =
  string_of_expr expr ^ ";\n"

let rec translateProgram = function
	| [] -> ""
	| stmnt :: tl -> (string_of_stmnt stmnt) ^ translateProgram tl





let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  translateProgram program
