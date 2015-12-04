open Ast

let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"
module SymTable = Map.Make (String)

(* Generate a C string to create a new monotype with the proper flags set. *)
let new_monotype_of_expr = function
    | IntLiteral(i) -> "new_monotype(0, " ^ string_of_int i ^
                     ", 0, 0, 0, NULL, 0);"
    | StrLiteral(str) -> "new_monotype(1, 0, " ^ str ^ ", 0, 0, NULL, 0);"
    | BoolLiteral(b) -> if b then "new_monotype(2, 0, 0, 1, 0, NULL, 0);"
                           else "new_monotype(2, 0, 0, 0, 0, NULL, 0);"
    | FloatLiteral(f) -> "new_monotype(3, 0, 0, 0, " ^ string_of_float f ^
                       ", NULL, 0);"

(* Translate an arg expression in the AST to a C expression, returning a string
   of that translation. *)
let arg_expr_to_c_expr = function
    | Assign(str, e) -> "struct monotype " ^ str ^ " = " ^ new_monotype_of_expr e
    | Call(str, el) -> ""
    | Id(str) -> ""
    | StrLiteral(str) -> "" 
    | Binop(e1, op, e2) -> ""
    | Noexpr -> raise Exit

(* Generate a C string to create a new monotype with a persistent array of
   monotypes stored within it.
   name: the array's user-specified name.
   len: the array's user-specifie length.
   el: an expression list, the evaluations of which comprise the array's contents*)
let new_monotype_array name len el =

    (* Using name ^ len to create an internal name is a hack. A user could in
       theory create an actual variable with that name. Need to use symtable? *)
    let tmpname = name ^ string_of_int len in

    (* First malloc a monotype array to store persistently in the stack
       monotype *)
    "struct monotype *" ^ tmpname ^
    " = malloc(sizeof(struct monotype) * "^ string_of_int len ^ ");\n" ^ 
    
    (* Load the evaluated results of the el into the malloc'd array in order. *)
    (fst
        (List.fold_left
            (fun p e ->
                (fst p) ^ tmpname ^ "[" ^ string_of_int (snd p) ^
                "] = " ^ new_monotype_of_expr e ^ ";\n", succ (snd p)
            )
            ("", 0) el
        )
    ) ^

    (* Call the new_monotype constructor with the array flag and send in the
       malloc'd and loaded array. *)
    "struct monotype " ^ name ^ "  = " ^
    "new_monotype(4, 0, NULL, 0, 0, " ^ tmpname ^ ", " ^
    string_of_int len ^ ");\n" 

(* Route an arg statement to its translator and return a string. *)
let rec arg_stmt_to_c_stmt = function
    | Expr(e) -> arg_expr_to_c_expr e
    | IfElse(e, s1, s2) -> ""
    | If(e, s) -> ""
    | While(e, s) -> ""
    | ArrayAssign(s, l, el) -> new_monotype_array s l el

(* Convert a list of arg statements to a string of C statements *)
let arg_body_to_c_body arg_body =
    List.map arg_stmt_to_c_stmt arg_body

(* Translate an arg function in the AST to a C function, returning a string of
   that translation. *)
let arg_func_to_c_func arg_func =
    "monotype " ^ arg_func.fname ^ "(" ^
    List.fold_left (fun a b -> a ^ b ^ ", ") "" arg_func.formals ^ ") {\n" ^ 
    List.fold_left (fun a b -> a ^ b ^ "\n") ""
    (List.map arg_stmt_to_c_stmt arg_func.body) ^ "\n}"

(* Route the functions and body segments of the program pair to their respective
   handlers and return the result as a pair of strings. *)
let translate_program arg =
    let arg_funcs = fst arg in
    let arg_body = snd arg in
    let c_funcs = List.map arg_func_to_c_func arg_funcs in
    let c_body = arg_body_to_c_body arg_body in
    (List.fold_left (fun a b -> a ^ b) "" c_funcs,
    List.fold_left (fun a b -> a ^ b) "" c_body)

(* Include necessary C libraries. Declare functions at top of file, then wrap
   body in a main function below.
   (* TODO: We shouldn't be including monotype.c, we should write its contents
      into the file. We obviously can't assume the user will have their own copy
      of monotype.c in their own directory! *)
*)
let wrap_program translated_program =
    let functions = fst translated_program in
    let body = snd translated_program in
    "#include <stdio.h>\n#include \"monotype.c\"\n" ^ functions ^
    "\n\nint main() {\n" ^ body ^ "\n\n\treturn 0;\n}\n"

let _ =
    let ic = open_in arg_file in
    let lexbuf = Lexing.from_channel ic in
    let arg = Parser.program Scanner.token lexbuf in
    let oc = open_out c_file in
    Printf.fprintf oc "%s\n" (wrap_program (translate_program arg));
    print_endline ("generated " ^ c_file);
    close_out oc;
    close_in ic;

