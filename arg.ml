open Ast

type types =
  | String of string

module SymTable = Map.Make (String)
let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"

let rec toStringList inp out =
  if(List.length inp > 0)
  then toStringList (List.tl inp) (out @ [(snd (List.hd inp))])
  else out

(* This is a fat TODO. Assume string for now because it's all we support. *)
let evalCall id params = String("hi")

let rec typeOfExpr e symTable =
  match e with
  | Assign(v, e) -> typeOfExpr(e)
  | Call(id, params) -> evalCall(id, params)
  | Id(s) -> SymTable.find s symTable
  | Noexpr ->  raise Exit (* WTF do we do about this? Raise an exception? *)
  | StrLiteral(l) -> String(l)

(* Returns a pair. The first element is a C string, the second is the symbol table
   in its proper state following the statement in the first element. *)
let rec string_of_expr expr symTable =
  match expr with
  | Assign(v, e) ->
    let symTable = SymTable.add v (typeOfExpr e symTable) symTable in
    let decPrefixStr = decTypeStr (SymTable.find v symTable) in
    (decPrefixStr ^ v ^ " = " ^ fst (string_of_expr e symTable), symTable)
  (* Below this line is TODO *)
  | Call(id, params) -> if (String.compare id "PRINT" == 0)
    then "Call", "printf" ^ "(" ^ String.concat ", " (toStringList
         (List.map string_of_expr params) []) ^ ")"
    else "Call", id ^ "(" ^ String.concat ", " (toStringList
         (List.map string_of_expr params) []) ^ ")"
  | StrLiteral(l) -> "StrLiteral", l
  | Id(s) -> "Id", s
  | Noexpr -> "Noexpr", ""

let rec string_of_stmnt expr symTable =
  (fst (string_of_expr expr symTable) ^ ";\n", snd (string_of_expr expr symTable))

let rec translateProgram program symTable =
  match program with
  | [] -> ""
  | stmnt :: tl -> fst (string_of_stmnt stmnt symTable) ^
                   translateProgram tl (snd (string_of_stmnt stmnt symTable))

(* wraps code in main function, with includes *)
let wrapProgram p =
  let indent line = "\t" ^ line in
  let lines = List.map indent (Str.split (Str.regexp "\n") p) in
  let body = List.fold_left (fun x y -> x ^ "\n" ^ y) "" lines in
  "#include <stdio.h>\n\nint main() {" ^ body ^ "\n\n\treturn 0;\n}\n"

let _ =
  let ic = open_in arg_file in
  let lexbuf = Lexing.from_channel ic in
  let program = Parser.program Scanner.token lexbuf in
  let oc = open_out c_file in
  let symTable = SymTable.empty in
  Printf.fprintf oc "%s\n" (wrapProgram (translateProgram program symTable));
  print_endline ("generated " ^ c_file);
  close_out oc;
  close_in ic;
