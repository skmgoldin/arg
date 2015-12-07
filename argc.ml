(* TODO: Add both global and function-level symbol tables to check that
   variables are in scope. We should consider whether we want to store types
   in the symtable as well, since doing so would let us generate a lot less code
   in some cases. *)
(* TODO: Figure out memory freeing for arrays. This will probably be reliant on
   our symbol table implementation. *)

(* ON THE SYMTABLE, A PROPOSAL: 
   If we send in the symtable to the assign and Id patterns in monotype_of_expr,
   that'll be sufficient in the to determine from the assign context if a variable
   needs to be created or merely updated; in the Id context, it will suffice to
   determine if a variable has ever been declared and, if not, force an error.

   But that is insufficient. We need to know our context when checking the symbol
   table: accessing body variables from function context must be disallowed and
   vice-versa, and creating new variables within Call and Binop context must be
   disallowed. 

   How about this: we build up the program string using a "buildup tuple":
   (s, c, symtable).

   All the code we've already written, which is building up a program string,
   will do its work on the fst member of this tuple, s. Then we do the symtable
   stuff seperately in the third member, symtable, and use the snd member, c, to
   provide context.

   The symtable uses as keys the str elements of the Id pattern. Its values are
   strings corresponding to a context: either "body" or the name of a function. 
   These contexts string are loaded into the c member of the buildup tuple by
   arg_body_to_cbody and arg_func_to_c_func, which know what context they are
   in. Then the buildup tuple is sent to monotype_of_expr, the patterns of which
   will then have all the information they need in the buildup tuple to properly
   handle variable scoping. *)

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
    | Binop(e1, op, e2) ->
        let arg_binop_to_c_binop e1 e2 = function
            | Add     -> "monotype_add(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Sub     -> "monotype_sub(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Mult     -> "monotype_mult(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Div     -> "monotype_div(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Equal     -> "monotype_equal(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Neq     -> "monotype_neq(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Less     -> "monotype_less(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Leq     -> "monotype_leq(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Greater     -> "monotype_greater(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Geq     -> "monotype_geq(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
        in arg_binop_to_c_binop e1 e2 op

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
    "if(" ^ monotype_of_expr expr ^ ".b) {\n" ^
    "printf(\"%s\\n\", \"True\");\n" ^
    "} else {\n" ^
    "printf(\"%s\\n\", \"False\");\n}" ^
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

let build_func_jump_table arg_func = ""

let expr_check_syms st arg_expr =
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
    | Binop(e1, op, e2) ->
        let arg_binop_to_c_binop e1 e2 = function
            | Add     -> "monotype_add(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Sub     -> "monotype_sub(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Mult     -> "monotype_mult(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Div     -> "monotype_div(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Equal     -> "monotype_equal(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Neq     -> "monotype_neq(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Less     -> "monotype_less(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Leq     -> "monotype_leq(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Greater     -> "monotype_greater(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
            | Geq     -> "monotype_geq(" ^ monotype_of_expr e1 ^ ", " ^ monotype_of_expr e2 ^ ")"
        in arg_binop_to_c_binop e1 e2 op

let rec stmt_check_syms st arg_stmt =
    | Expr(e) -> expr_check_syms st e
    | IfElse(e, s1, s2) ->
        let st = expr_check_syms st e in
        let st = stmt_check_syms st s1 in
        let st = stmt_check_syms st s2
    | If(e, s) ->
        let st = expr_check_syms st e in
        let st = stmt_check_syms st s
    | While(e, s) ->
        let st = expr_check_syms st e in
        let st = stmt_check_syms st s
    | ArrayAssign(s, l, el) ->
        let st = expr_check_syms st e in
        List.append st s
    | Print(s, e) ->
        expr_check_syms st e

let build_func_sym_table arg_func =
    let st = [] in
    let st = List.fold_left (fun a b -> List.append a b) st arg_func.formals in
    List.fold_left stmt_check_syms st arg_func.body

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
  (*let (c_funcs, _, jt) = List.fold_left arg_func_to_c_func ([], []) arg_funcs in (* c_funcs, jt *)*)
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
