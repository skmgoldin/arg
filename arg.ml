open Ast

(*
module SymTable = Map.Make (String)
let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"

type monotype =
  | Integer of int
  | String of string
  | Boolean of bool
  | Float of float

(*
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
*)

let rec string_of_expr_list l =
  c_of_expr (List.hd l) ^ ", " ^ string_of_expr_list (List.tl l)

let rec literal_to_monotype = function
  | Call(id, params) -> id ^ "(" ^ string_of_expr_list params ^ ");"
  | Id(s) -> s ^ ";"
  | Noexpr -> raise Exit
  | StrLiteral(l) -> "new_monotype(1, 0, " ^ l ^ ", 0, 0.0);"
  | Assign(v, e) -> v ^ " = " ^ literal_to_monotype e

(* Get the evaluated type of an expression. *)
(*
let rec typeOfExpr e symTable =
  match e with
  | Call(id, params) -> monotype
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
*)

let assignType monotype =
  if monotype.isint = true then "int" else
  if monotype.isstring = true then "char *" else
  if monotype.isbool = true then "int" else
  if monotype.isfloat = true then "float" else raise Exit

(* Returns a pair. The first element is a C string, the second is the symbol table
   in its proper state following the statement in the first element. *)
let rec c_of_expr expr symTable =
  match expr with
  | Assign(v, e) ->
    if SymTable.find v symTable (* If v is already in the symtable *)
    then let symTable = SymTable.add v v symTable in
      let assignPrefix = "" in
      (assignPrefixStr ^ v ^ " = " ^ fst (c_of_expr e symTable), symTable)
    (* If v is not in the symtable already, we need to declare a new monotype *)
    else let symTable = SymTable.add v v symTable in
      let assignPrefixStr = assignType (typeOfExpr e symTable) in
      ("struct monotype " ^ v ^ " = " ^ fst (c_of_expr e symTable), symTable)
  (* Below this line is TODO *)
  | Call(id, params) -> if (String.compare id "PRINT" == 0)
    then "Call", "printf" ^ "(" ^ String.concat ", " (toStringList
         (List.map c_of_expr params) []) ^ ")"
    else "Call", id ^ "(" ^ String.concat ", " (toStringList
         (List.map c_of_expr params) []) ^ ")"
  | StrLiteral(l) -> "StrLiteral", l
  | Id(s) -> "Id", s
  | Noexpr -> "Noexpr", ""

let rec c_of_stmnt expr symTable =
  (fst (c_of_expr expr symTable) ^ ";\n", snd (c_of_expr expr symTable))

let divide_functions arg =
  ;

(* returns tuple of C code -- first elem is main body, second elem is function defs *)
(*
let rec translateProgram symTable = function
  | [] -> ("", "")
  | stmnt :: tl -> (fst (c_of_stmnt stmnt symTable) ^
                   translateProgram tl (snd (c_of_stmnt stmnt symTable)), "")
*)
let translateProgram arg =
  let arg_functions, arg_body = divide_functions arg in
  (translate_functions arg_functions, translate_body arg_body)

(* wraps code in main function, with includes *)
let wrapProgram functions body =
  "#include <stdio.h>\n\n" ^ functions ^ "\n\nint main() {" ^ body ^ "\n\n\treturn 0;\n}\n"


let _ =
  let ic = open_in arg_file in
  let lexbuf = Lexing.from_channel ic in
  let arg = Parser.program Scanner.token lexbuf in
  let oc = open_out c_file in
  Printf.fprintf oc "%s\n" (wrapProgram (translateProgram arg));
  print_endline ("generated " ^ c_file);
  close_out oc;
  close_in ic;

*)

let _ =
  let ic = open_in arg_file in
  let lexbuf = Lexing.from_channel ic in
  let arg = Parser.program Scanner.token lexbuf in
  let oc = open_out c_file in
  print_endline ("done");
  close_out oc;
  close_in ic;
