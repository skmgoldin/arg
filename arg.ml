open Ast

let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"
module SymTable = Map.Make (String)

(* THESE FUNCTIONS/TYPES MARKED FOR DEATH *)
(*
type monotype =
  | Integer of int
  | String of string
  | Boolean of bool
  | Float of float

let assign_type monotype =
  if monotype.isint = true then "int" else
  if monotype.isstring = true then "char *" else
  if monotype.isbool = true then "int" else
  if monotype.isfloat = true then "float" else raise Exit

(* Returns a pair. The first element is a C string, the second is the symbol table
   in its proper state following the statement in the first element. *)
let rec c_of_expr expr sym_table =
  match expr with
  | Assign(v, e) ->
    if SymTable.find v sym_table (* If v is already in the symtable *)
    then let sym_table = SymTable.add v v sym_table in
      let assignPrefix = "" in
      (assignPrefixStr ^ v ^ " = " ^ fst (c_of_expr e sym_table), sym_table)
    (* If v is not in the symtable already, we need to declare a new monotype *)
    else let sym_table = SymTable.add v v sym_table in
      let assignPrefixStr = assign_type (typeOfExpr e sym_table) in
      ("struct monotype " ^ v ^ " = " ^ fst (c_of_expr e sym_table), sym_table)
  (* Below this line is TODO *)
  | Call(id, params) -> if (String.compare id "PRINT" == 0)
    then "Call", "printf" ^ "(" ^ String.concat ", " (to_string_list
         (List.map c_of_expr params) []) ^ ")"
    else "Call", id ^ "(" ^ String.concat ", " (to_string_list
         (List.map c_of_expr params) []) ^ ")"
  | StrLiteral(l) -> "StrLiteral", l
  | Id(s) -> "Id", s
  | Noexpr -> "Noexpr", ""

let rec string_of_expr_list l helper =
  c_of_expr (List.hd l) ^ ", " ^ string_of_expr_list (List.tl l)

let rec literal_to_monotype = function
  | Call(id, params) -> id ^ "(" ^ string_of_expr_list params ^ ");"
  | Id(s) -> s ^ ";"
  | Noexpr -> raise Exit
  | StrLiteral(l) -> "new_monotype(1, 0, " ^ l ^ ", 0, 0.0);"
  | Assign(v, e) -> v ^ " = " ^ literal_to_monotype e

let rec c_of_stmnt expr sym_table =
  (fst (c_of_expr expr sym_table) ^ ";\n", snd (c_of_expr expr sym_table))
*)
(* END DEATH ZONE *)

let arg_body_to_c_body arg_body =
  "THE BODY"

let arg_func_to_c_func arg_func =
  "monotype " ^ arg_func.fname ^ "(" ^
  List.fold_left (fun a b -> a ^ b ^ ", ") "" arg_func.formals ^ ") {\n" ^ 
  arg_statement_to_c_statement arg_func.statement ^ "\n}"

(* Route the functions and body segments of the program pair to their respective
   handlers and return the result as a pair of strings. *)
let translate_program arg =
  let arg_funcs = fst arg in
  let arg_body = snd arg in
  let c_funcs = List.map arg_func_to_c_func arg_funcs in
  let c_body = arg_body_to_c_body arg_body in
  (List.fold_left (fun a b -> a ^ b) "" c_funcs, c_body)

(* Include necessary C libraries. Declare functions at top of file, then wrap
   body in a main function below.*)
let wrap_program translated_program =
  let functions = fst translated_program in
  let body = snd translated_program in
  "#include <stdio.h>\n\n" ^ functions ^ "\n\nint main() {" ^ body ^ "\n\n\treturn 0;\n}\n"

let _ =
  let ic = open_in arg_file in
  let lexbuf = Lexing.from_channel ic in
  let arg = Parser.program Scanner.token lexbuf in
  let oc = open_out c_file in
  Printf.fprintf oc "%s\n" (wrap_program (translate_program arg));
  print_endline ("generated " ^ c_file);
  close_out oc;
  close_in ic;

