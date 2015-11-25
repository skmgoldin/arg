open Ast

module SymTable = Map.Make (String)
let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"

type monotype =
{
  isint: bool;
  i: int;

  isstring: bool;
  s: string;

  isbool: bool;
  b: bool;

  isfloat: bool;
  f: float;
}

let emptyMonoType =
{
  isint = false;
  i = 0;

  isstring = false;
  s = "";

  isbool = false;
  b = false;

  isfloat = false;
  f = 0.0;
}

(* Get the evaluated type of an expression. *)
let rec typeOfExpr e symTable =
  match e with
  | Call(id, params) -> emptyMonoType
  | Id(s) -> if SymTable.is_empty symTable then raise Exit else SymTable.find s symTable
  | Noexpr ->  raise Exit
  | StrLiteral(l) ->
    {
      isint = false;
      i = 0;

      isstring = true;
      s = l;

      isbool = false;
      b = false;

      isfloat = false;
      f = 0.0;
    }
  | Assign(v, e) -> typeOfExpr e symTable

let decTypeStr monotype =
  if monotype.isint = true then "int" else 
  if monotype.isstring = true then "char *" else 
  if monotype.isbool = true then "int" else 
  if monotype.isfloat = true then "float" else raise Exit

(* Returns a pair. The first element is a C string, the second is the symbol table
   in its proper state following the statement in the first element. *)
let rec string_of_expr expr symTable =
  match expr with
  | Assign(v, e) ->
    let symTable = SymTable.add v (typeOfExpr e symTable) symTable in
    (* decPrefixStr: the C prefix of the var declaration, ie "char *" or "int" *)
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
