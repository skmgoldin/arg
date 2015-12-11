open Ast

let arg_file = Sys.argv.(1) ^ ".arg"
let c_file = Sys.argv.(1) ^ ".c"

type scope_entity = Function | Variable

(* Utility function, return a pair, second of which is true if the given list
   contains the given string and false otherwise. *)
let list_contains_string l s =
    List.fold_left (fun a b ->
        if ((snd a) || (String.compare (fst a) (fst b)) = 0) then (s, true) else (s, false))
        (s, false) l

(* Utility function, return a pair, second of which is true if the given list
   contains the given string and that string is a Variable. false otherwise. *)
let element_is_variable l s =
    List.fold_left (fun a b ->
        if ((snd a) || ((String.compare (fst a) (fst b)) = 0) && (snd b = Variable))
        then (s, true)
        else (s, false)
    )
    (s, false) l

(* Because everything in ARG must be represented by a C monotype, this function
   should return a pair, the first of which is a string of valid C code which
   will evaluate in C to a struct monotype. The second of the pair is the
   symbol table. *)
let rec monotype_of_expr expr st =
    match expr with
    | IntLiteral(i) -> ("new_monotype(0, " ^ string_of_int i ^
                     ", 0, 0, 0, NULL, 0)", st)
    | StrLiteral(str) -> ("new_monotype(1, 0, " ^ str ^ ", 0, 0, NULL, 0)", st)
    | BoolLiteral(b) -> if b then ("new_monotype(2, 0, 0, 1, 0, NULL, 0)", st)
                           else ("new_monotype(2, 0, 0, 0, 0, NULL, 0)", st)
    | FloatLiteral(f) -> ("new_monotype(3, 0, 0, 0, " ^ string_of_float f ^
                       ", NULL, 0)", st)
    | Assign(str, e) ->
        if (snd (list_contains_string st str) && snd (element_is_variable st str))
        then let (rhs, st) = monotype_of_expr e st in
            (str ^ " = " ^ rhs, st)
        else if snd (list_contains_string st str)
        then (print_string
             ("You are trying to assign to a function. Error.\n"); raise Exit)
        else let (rhs, st) = monotype_of_expr e st in
            ("struct monotype " ^ str ^ " = " ^ rhs, st @ [(str, Variable)])
    | Call(str, el) ->
        if (snd (list_contains_string st str) && not (snd (element_is_variable st str)))
        then
            let arglist =
                List.fold_left (fun s e -> s ^ fst (monotype_of_expr e st) ^ ", ") "" el in
            (* Arglist has an extra comma and space at its end. Remove them below. *)
            let strlen = String.length arglist in
            let arglist = if String.length arglist != 0 then
            String.sub arglist 0 (strlen - 2) else "" in
            (str ^ "(" ^ arglist ^ ")", st)
        else if snd (list_contains_string st str)
        then (print_string
             ("You are trying to call a variable as a function. Error.\n"); raise Exit)
        else (print_string
             ("A function is called which does not exist. Error.\n"); raise Exit)
    | Id(str) ->
        if snd (list_contains_string st str)
        then (str, st)
        else (print_string
            ("An ID is used which does not exist. Error.\n"); raise Exit)
    | ArrId(str, i) ->
        if snd (list_contains_string st str)
        then (str ^ ".a[" ^ string_of_int i ^ "]", st)
        else (print_string
            ("An ID is used which does not exist. Error.\n"); raise Exit)
    | Binop(e1, op, e2) ->
        let arg_binop_to_c_binop e1 e2 = function
            | Add     -> ("monotype_add(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                          fst (monotype_of_expr e2 st) ^ ")", st)
            | Sub     -> ("monotype_sub(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                          fst (monotype_of_expr e2 st) ^ ")", st)
            | Mult     -> ("monotype_mult(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                           fst (monotype_of_expr e2 st) ^ ")", st)
            | Div     -> ("monotype_div(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                          fst (monotype_of_expr e2 st) ^ ")", st)
            | Equal     -> ("monotype_equal(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                            fst (monotype_of_expr e2 st) ^ ")", st)
            | Neq     -> ("monotype_neq(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                          fst (monotype_of_expr e2 st) ^ ")", st)
            | Less     -> ("monotype_less(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                           fst (monotype_of_expr e2 st) ^ ")", st)
            | Leq     -> ("monotype_leq(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                          fst (monotype_of_expr e2 st) ^ ")", st)
            | Greater     -> ("monotype_greater(" ^ fst (monotype_of_expr e1 st) ^
                             ", " ^ fst (monotype_of_expr e2 st) ^ ")", st)
            | Geq     -> ("monotype_geq(" ^ fst (monotype_of_expr e1 st) ^ ", " ^
                          fst (monotype_of_expr e2 st) ^ ")", st)
        in arg_binop_to_c_binop e1 e2 op

(* Generate a C string to create a new monotype with a persistent array of
   monotypes stored within it.
   name: the array's user-specified name.
   len: the array's user-specifie length.
   el: an expression list, the evaluations of which comprise the array's contents*)
let new_monotype_array name len el st =

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
                "] = " ^ fst (monotype_of_expr e st) ^ ";\n", succ (snd p)
            )
            ("", 0) (List.rev el)
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
let arg_print_to_c_print fmt expr st =
    "if(" ^ fst (monotype_of_expr expr st) ^ ".isint) {\n" ^
    "printf(\"%d\\n\", " ^ fst (monotype_of_expr expr st) ^ ".i);\n" ^
    "} else if(" ^ fst (monotype_of_expr expr st) ^ ".ischar) {\n" ^
    "printf(\"%s\\n\", " ^ fst (monotype_of_expr expr st) ^ ".s);\n" ^
    "} else if(" ^ fst (monotype_of_expr expr st) ^ ".isbool) {\n" ^
    "if(" ^ fst (monotype_of_expr expr st) ^ ".b) {\n" ^
    "printf(\"%s\\n\", \"True\");\n" ^
    "} else {\n" ^
    "printf(\"%s\\n\", \"False\");\n}" ^
    "} else if(" ^ fst (monotype_of_expr expr st) ^ ".isfloat) {\n" ^
    "printf(\"%f\\n\", " ^ fst (monotype_of_expr expr st) ^ ".f);\n" ^
    "} else { printf(\"%s\\n\", \"Error!\"); }"

(* Add a new, unique label to the head of the jump table. Return the new jump
   table. *)
let add_label_to_jt jt =
    if List.length jt = 0 then ["j0"] else
    let labelnum = List.length jt in
    let label = "j" ^ string_of_int labelnum in
    label :: jt

(* Route an arg statement to its translator and return a triple with a valid C 
   string, the jump table and the symbol table. *)
let rec arg_stmt_to_c_stmt stmt jt st =
    match stmt with
    | Expr(e) ->
        let (str, st) = monotype_of_expr e st in
        (str ^ ";\n", jt, st)
    | IfElse(e, s1, s2) ->
        let (if_body, jt, st) =
            List.fold_left
                (fun a b ->
                    let (sl, jt, st) = a in
                    let (stmt, jt, st) = arg_stmt_to_c_stmt b jt st in
                    (sl @ [stmt], jt, st)
                )
            ([], jt, st) s1
        in
        let (else_body, jt, st) =
            List.fold_left
                (fun a b ->
                    let (sl, jt, st) = a in
                    let (stmt, jt, st) = arg_stmt_to_c_stmt b jt st in
                    (sl @ [stmt], jt, st)
                )
            ([], jt, st) s2
        in
        ("if(" ^ fst (monotype_of_expr e st) ^ ".b) {\n" ^
        (List.fold_left (fun a b -> a ^ b) "" if_body)
        ^ "} else {\n" ^ (List.fold_left (fun a b -> a ^ b) "" else_body) ^ "}"
        , jt, st)

    | If(e, s) ->
        let (if_body, jt, st) =
            List.fold_left
                (fun a b ->
                    let (sl, jt, st) = a in
                    let (stmt, jt, st) = arg_stmt_to_c_stmt b jt st in
                    (sl @ [stmt], jt, st)
                )
            ([], jt, st) s
        in
        ("if(" ^ fst (monotype_of_expr e st) ^ ".b) {\n" ^
        (List.fold_left (fun a b -> a ^ b) "" if_body)
        ^ "}", jt, st)
    | While(e, s) ->
        let jt = add_label_to_jt jt in
        let label = List.hd jt in
        let (while_body, jt, st) =
            List.fold_left
                (fun a b ->
                    let (sl, jt, st) = a in
                    let (stmt, jt, st) = arg_stmt_to_c_stmt b jt st in
                    (sl @ [stmt], jt, st)
                )
            ([], jt, st) s
        in
        (
            label ^ ":;\nif(" ^ fst (monotype_of_expr e st) ^ ".b) {\n" ^
            (List.fold_left (fun a b -> a ^ b) "" while_body) ^
            "goto " ^ label ^ ";}",
            jt, st
        )
    | ArrayAssign(s, l, el) ->
        (new_monotype_array s l el st ^ "\n", jt, st @ [(s, Variable)])
      
    | Print(s, e) -> (arg_print_to_c_print s e st ^ "\n", jt, st)

(* Convert a list of arg statements to a list of valid C strings. Return that
   list, the jump table and the symbol table. *)
let arg_body_to_c_body arg_body jt st =
    List.fold_left
        (fun a b ->
            let (sl, jt, st) = a in
            let (stmt, jt, st) = arg_stmt_to_c_stmt b jt st in
            (sl @ [stmt], jt, st)
        )
    ([], jt, st) arg_body

(* Translate an arg function in the AST to a C function, returning a string of
   that translation along with the jump table and symbol table. *)
let arg_func_to_c_func arg_func jt st =
    let arglist =
        List.fold_left (fun a b -> a ^ "struct monotype " ^ b ^ ", ") "" arg_func.formals in
    (* Arglist has an extra comma and space at its end. Remove them below. *)
    let strlen = String.length arglist in
    let arglist = if String.length arglist != 0 then
        String.sub arglist 0 (strlen - 2) else "" in

    let (func_body, jt, st) =
        List.fold_left
            (fun a b ->
                let (sl, jt, st) = a in
                let (stmt, jt, st) = arg_stmt_to_c_stmt b jt st in
                (sl @ [stmt], jt, st)
            )
        ([], jt, st) arg_func.body
    in
    (
        "struct monotype " ^ arg_func.fname ^ "(" ^ arglist ^ ") {\n" ^
        (List.fold_left (fun a b -> a ^ b) "" func_body) ^ "\n}",
        jt, st
    )

(* Utility function. Remove all scope_entities with the Variable type from the
   symbol table. Call this after translating functions. *)
let remove_vars_from_st st =
    List.fold_left
        (fun a b ->
            if snd b = Function
            then a @ [b]
            else a
        )
        [] st

(* Route the functions and body segments of the program pair to their respective
   handlers and return the result as a pair of strings. *)
let translate_program arg =
    (* Name working elements. *)
    let arg_funcs = fst arg in
    let arg_body = snd arg in

    (* Convert ARG to C. *)
    let (c_funcs, jt, st) =
        List.fold_left
        (fun a b ->
                let (sl, jt, st) = a in
                let (stmt, jt, st) = arg_func_to_c_func b jt
                    (remove_vars_from_st st) in
                (sl @ [stmt], jt, st)
        )
        ([], [], []) arg_funcs
    in
    let st = remove_vars_from_st st in
    let (c_body, jt, st) = arg_body_to_c_body arg_body jt st in
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
