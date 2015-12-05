open Ast

let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"
module SymTable = Map.Make (String)

(* Because everything in ARG must be represented by a C monotype, this function
   should return strings of valid C code which will evaluate in C to a struct
   monotype. *)
let rec monotype_of_expr = function
    | IntLiteral(i) -> "new_monotype(0, " ^ string_of_int i ^
                     ", 0, 0, 0, NULL, 0)"
    | StrLiteral(str) -> "new_monotype(1, 0, " ^ str ^ ", 0, 0, NULL, 0)"
    | BoolLiteral(b) -> if b then "new_monotype(2, 0, 0, 1, 0, NULL, 0)"
                           else "new_monotype(2, 0, 0, 0, 0, NULL, 0)"
    | FloatLiteral(f) -> "new_monotype(3, 0, 0, 0, " ^ string_of_float f ^
                       ", NULL, 0)"
    | Assign(str, e) -> "struct monotype " ^ str ^ " = " ^ monotype_of_expr e
    | Call(str, el) ->
        let arglist =
            List.fold_left (fun s e -> s ^ monotype_of_expr e ^ ", ") "" el in
        (* Arglist has an extra comma and space at its end. Remove them below. *)
        let strlen = String.length arglist in
        let arglist = String.sub arglist 0 (strlen - 2) in
        str ^ "(" ^ arglist ^ ")"
    | Id(str) -> str
    | Binop(e1, op, e2) -> "BINOP" (* TODO *)
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
                "] = " ^ monotype_of_expr e ^ ";\n", succ (snd p)
            )
            ("", 0) el
        )
    ) ^

    (* Call the new_monotype constructor with the array flag and send in the
       malloc'd and loaded array. *)
    "struct monotype " ^ name ^ "  = " ^
    "new_monotype(4, 0, NULL, 0, 0, " ^ tmpname ^ ", " ^
    string_of_int len ^ ");\n" 

(* Generate an if/else block that directs a monotype to the appropriate printf
   format string.
   (* TODO: we should support PRINT statements with multiple arguments, so we'll
   need to build up the format strings in a smarter way, possibly by evaluating
   a list of expressions and programmatically building up fmt strings based on
   their types. *) *)
let arg_print_to_c_print fmt expr =
    "if(" ^ monotype_of_expr expr ^ ".isint) {\n" ^
    "printf(\"%d\\n\", " ^ monotype_of_expr expr ^ ".i);\n" ^
    "} else if(" ^ monotype_of_expr expr ^ ".ischar) {\n" ^
    "printf(\"%s\\n\", " ^ monotype_of_expr expr ^ ".s);\n" ^
    "} else if(" ^ monotype_of_expr expr ^ ".isbool) {\n" ^
    "printf(\"%s\\n\", " ^ monotype_of_expr expr ^ ".b);\n" ^
    "} else if(" ^ monotype_of_expr expr ^ ".isfloat) {\n" ^
    "printf(\"%f\\n\", " ^ monotype_of_expr expr ^ ".f);\n" ^
    "} else { printf(\"%s\\n\", \"Error!\"); }"

(* Route an arg statement to its translator and return a string. *)
let rec arg_stmt_to_c_stmt = function
    | Expr(e) -> monotype_of_expr e ^ ";\n"
    | IfElse(e, s1, s2) -> "" ^ "\n"
    | If(e, s) -> "" ^ "\n"
    | While(e, s) -> "" ^ "\n"
    | ArrayAssign(s, l, el) -> new_monotype_array s l el ^ "\n"
    | Print(s, e) -> arg_print_to_c_print s e ^ "\n"

(* Convert a list of arg statements to a string of C statements *)
let arg_body_to_c_body arg_body =
    List.map arg_stmt_to_c_stmt arg_body

(* Translate an arg function in the AST to a C function, returning a string of
   that translation. *)
let arg_func_to_c_func arg_func =
    let arglist =
        List.fold_left (fun a b -> a ^ "struct monotype " ^ b ^ ", ") "" arg_func.formals in
    (* Arglist has an extra comma and space at its end. Remove them below. *)
    let strlen = String.length arglist in
    let arglist = String.sub arglist 0 (strlen - 2) in

    "struct monotype " ^ arg_func.fname ^ "(" ^ arglist ^ ") {\n" ^ 
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

