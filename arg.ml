open Ast

let file = "helloworld.arg"

let rec string_of_expr = function
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(id, params) -> id ^ "(" ^ string_of_expr (List.hd params) ^ ");"
  | StrLiteral(l) -> l
  | Id(s) -> s
  | Noexpr -> ""

let rec string_of_stmnt expr =
  string_of_expr expr ^ ";\n"

let rec translateProgram = function
  | [] -> ""
  | stmnt :: tl -> (string_of_stmnt stmnt) ^ translateProgram tl

let _ =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let program = Parser.program Scanner.token lexbuf in
  print_endline (translateProgram program)
