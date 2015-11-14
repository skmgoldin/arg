open Ast

let file = "helloworld.arg"

let rec toStringList inp out =
  if(List.length inp > 0) then
  toStringList (List.tl inp) (out @ [(snd (List.hd inp))])
  else out

let rec string_of_expr = function
  | Assign(v, e) -> if ((String.compare (fst (string_of_expr e)) "StrLiteral") == 0) then
    "Assign", "char *" ^ v ^ " = " ^ snd (string_of_expr e)
    else "Assign", v ^ " = " ^ snd (string_of_expr e)
  | Call(id, params) -> if (String.compare id "PRINT" == 0) then
    "Call", "printf" ^ "(" ^ String.concat ", " (toStringList (List.map string_of_expr params) []) ^ ")"
    else "Call", id ^ "(" ^ String.concat ", " (toStringList (List.map string_of_expr params) []) ^ ")"
  | StrLiteral(l) -> "StrLiteral", l
  | Id(s) -> "Id", s
  | Noexpr -> "Noexpr", ""

let rec string_of_stmnt expr =
  snd (string_of_expr expr) ^ ";\n"

let rec translateProgram = function
  | [] -> ""
  | stmnt :: tl -> (string_of_stmnt stmnt) ^ translateProgram tl

let _ =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let program = Parser.program Scanner.token lexbuf in
  print_endline (translateProgram program)
