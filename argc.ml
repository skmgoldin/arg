(* TODO: Allow use of array elements with a[5] syntax in expressions. *)
(* TODO: While loops *)
(* TODO: conditionals *)

open Ast

let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"

type scope_entity = Function | Variable

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
        let arglist = if String.length arglist != 0 then
        String.sub arglist 0 (strlen - 2) else "" in
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
let rec arg_stmt_to_c_stmt stmt jt =
    match stmt with
    | Expr(e) -> (monotype_of_expr e ^ ";\n", jt)
    | IfElse(e, s1, s2) -> ("" ^ "\n", jt)
    | If(e, s) -> ("" ^ "\n", jt)
    | While(e, s) -> ("" ^ "\n", jt)
    | ArrayAssign(s, l, el) -> (new_monotype_array s l el ^ "\n", jt)
    | Print(s, e) -> (arg_print_to_c_print s e ^ "\n", jt)

(* Convert a list of arg statements to a string of C statements *)
let arg_body_to_c_body arg_body jt =
    List.fold_left
        (fun a b ->
            let (stmt, jt) = arg_stmt_to_c_stmt b (snd a) in
            ((fst a) @ [stmt], jt)
        )
    ([], jt) arg_body

(* Utility function, return a pair, second of which is true if the given list
   contains the given string and false otherwise. *)
let list_contains_string l s =
    List.fold_left (fun a b ->
        if ((snd a) || (String.compare (fst a) (fst b)) = 0) then (s, true) else (s, false))
        (s, false) l

(* Check if the string s is in the list l and if so, if it is a variable. *)
let element_is_variable l s =
    List.fold_left (fun a b ->
        if ((snd a) || ((String.compare (fst a) (fst b)) = 0) && (snd b = Variable))
        then (s, true)
        else (s, false)
    )
    (s, false) l

(* Do bookkeeping on the symbol table in expression context and note lookup
   failures where relevant. *)
let rec expr_check_syms st arg_expr =
    match arg_expr with
    | IntLiteral(i) -> st
    | StrLiteral(str) -> st
    | BoolLiteral(b) -> st 
    | FloatLiteral(f) -> st
    | Assign(str, e) -> let st = expr_check_syms st e in
        if (snd (list_contains_string st str) && snd (element_is_variable st str))
        then st
        else if snd (list_contains_string st str)
        then (print_string
        ("Cannot assign to an in-use function name. Error.\n"); raise Exit)
        else List.append st [(str, Variable)]
    | Call(str, el) ->
        let _ = if (snd (list_contains_string st str) && not (snd (element_is_variable st str)))
        then st
        else if snd (list_contains_string st str)
        then (print_string
             ("You are trying to call a variable as a function. Error.\n"); raise Exit)
        else (print_string
             ("A function is called which does not exist. Error.\n"); raise Exit)
        in
        List.fold_left expr_check_syms st el
    | Id(str) -> if snd (list_contains_string st str) then st else (print_string
        ("An ID is used which does not exist. Error.\n"); raise Exit)
    | Binop(e1, op, e2) ->
        let _ = expr_check_syms st e1 in
        expr_check_syms st e2

(* Do bookkeeping on the symbol table in statement context and note lookup
   failures where relevant. *)
let rec stmt_check_syms st arg_stmt =
    match arg_stmt with
    | Expr(e) -> expr_check_syms st e
    | IfElse(e, s1, s2) ->
        let st = expr_check_syms st e in
        let st = List.fold_left stmt_check_syms st s1 in
        List.fold_left stmt_check_syms st s2
    | If(e, s) ->
        let st = expr_check_syms st e in
        List.fold_left stmt_check_syms st s
    | While(e, s) ->
        let st = expr_check_syms st e in
        List.fold_left stmt_check_syms st s
    | ArrayAssign(s, l, el) ->
        let st = List.fold_left expr_check_syms st el in
        List.append st [(s, Variable)]
    | Print(s, e) ->
        expr_check_syms st e


let build_func_sym_table arg_func =
    let st = [] in
    (* Add function params to symbol table *)
    let st = List.fold_left (fun a b ->
        if (snd (list_contains_string a b)) then (print_string ("Function " ^
        "parameters with matching names. Error.\n"); raise Exit) else List.append a [(b, Variable)])
        st arg_func.formals
    in
    (* With param name context in the symbol table, check the function body. *)
    List.fold_left stmt_check_syms st arg_func.body

(* Translate an arg function in the AST to a C function, returning a string of
   that translation. *)
let arg_func_to_c_func arg_func jt =
    let arglist =
        List.fold_left (fun a b -> a ^ "struct monotype " ^ b ^ ", ") "" arg_func.formals in
    (* Arglist has an extra comma and space at its end. Remove them below. *)
    let strlen = String.length arglist in
    let arglist = if String.length arglist != 0 then
        String.sub arglist 0 (strlen - 2) else "" in

    let (func_body, jt) =
        List.fold_left
            (fun a b ->
                 let (stmt, jt) = arg_stmt_to_c_stmt b (snd a) in
                 ((fst a) @ [stmt], jt)
            )
        ([], jt) arg_func.body
    in
    (
        "struct monotype " ^ arg_func.fname ^ "(" ^ arglist ^ ") {\n" ^
        (List.fold_left (fun a b -> a ^ b) "" func_body) ^ "\n}",
        jt
    )

(* Route the functions and body segments of the program pair to their respective
   handlers and return the result as a pair of strings. *)
let translate_program arg =
    (* Name working elements. *)
    let arg_funcs = fst arg in
    let arg_body = snd arg in

    (* Check variable scope conformity. *)
    let _ = List.map build_func_sym_table arg_funcs in
    let st = List.fold_left (fun a b -> List.append a [(b.fname, Function)]) [] arg_funcs in
    let st = List.fold_left stmt_check_syms st arg_body in

    (* Convert ARG to C. *)
    let (c_funcs, jt) =
        List.fold_left
        (fun a b ->
                    let (func, jt) = arg_func_to_c_func b (snd a) in
                    ((fst a) @ [func], jt))
        ([], []) arg_funcs
    in
    let (c_body, jt) = arg_body_to_c_body arg_body jt in
    (
        List.fold_left (fun a b -> a ^ b) "" c_funcs,
        List.fold_left (fun a b -> a ^ b) "" c_body,
        st
    )

let generate_free_block st =
    List.fold_left (fun a b ->
        if (snd b) = Variable
        then "if(" ^ (fst b) ^ ".isarray) { free(" ^ (fst b) ^ ".a); }\n"
        else "") "" st

(* Include necessary C libraries. Declare functions at top of file, then wrap
   body in a main function below.
   (* TODO: We shouldn't be including monotype.c, we should write its contents
      into the file. We obviously can't assume the user will have their own copy
      of monotype.c in their own directory! *)
*)
let wrap_program translated_program =
    let (functions,_,_) = translated_program in
    let (_,body,_) = translated_program in
    let (_,_,st) = translated_program in
    let free_block = generate_free_block st in
    "#include <stdio.h>\n#include \"monotype.c\"\n" ^ functions ^
    "\n\nint main() {\n" ^ body ^ "\n\n" ^ free_block ^ "\treturn 0;\n}\n"

let _ =
    let ic = open_in arg_file in
    let lexbuf = Lexing.from_channel ic in
    let arg = Parser.program Scanner.token lexbuf in
    let oc = open_out c_file in
    Printf.fprintf oc "%s\n" (wrap_program (translate_program arg));
    print_endline ("generated " ^ c_file);
    close_out oc;
    close_in ic;
