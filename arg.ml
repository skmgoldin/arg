open Ast
open Str

let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"

let rec toStringList inp out =
  if(List.length inp > 0)
  then toStringList (List.tl inp) (out @ [(snd (List.hd inp))])
  else out

let rec string_of_expr = function
  | Assign(v, e) ->
    if ((String.compare (fst (string_of_expr e)) "StrLiteral") == 0)
    then "Assign", "char *" ^ v ^ " = " ^ snd (string_of_expr e)
    else "Assign", v ^ " = " ^ snd (string_of_expr e)
  | Call(id, params) -> if (String.compare id "PRINT" == 0)
    then "Call", "printf" ^ "(" ^ String.concat ", " (toStringList
         (List.map string_of_expr params) []) ^ ")"
    else "Call", id ^ "(" ^ String.concat ", " (toStringList
         (List.map string_of_expr params) []) ^ ")"
  | StrLiteral(l) -> "StrLiteral", l
  | Id(s) -> "Id", s
  | Noexpr -> "Noexpr", ""

let rec string_of_stmnt expr =
  snd (string_of_expr expr) ^ ";\n"

let rec translateProgram = function
  | [] -> ""
  | stmnt :: tl -> (string_of_stmnt stmnt) ^ translateProgram tl

let indent line = "\t" ^ line

let wrapProgram p =
  let lines = List.map indent (Str.split (Str.regexp "\n") p) in
  let body = List.fold_left (fun x y -> x ^ "\n" ^ y) "" lines in
  "#include <stdio.h>\n\nint main() {" ^ body ^ "\n\n\treturn 0;\n}\n"

let _ =
  let ic = open_in arg_file in
  let lexbuf = Lexing.from_channel ic in
  let program = Parser.program Scanner.token lexbuf in
  let oc = open_out c_file in
  Printf.fprintf oc "%s\n" (wrapProgram (translateProgram program));
  print_endline ("generated " ^ c_file);
  close_out oc;
  close_in ic;
