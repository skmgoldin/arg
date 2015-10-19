{ open Parser }

rule token = parse
	  [' ' '\t' '\r' '\n'] { token lexbuf }
	| ['A'-'Z' 'a'-'z']+ as lit { VARIABLE(int_of_char lit - 97) }  (* Special case for capitals *)
	| ['0'-'9']+ as intlit { LITERAL(int_of_string intlit) }
	| " { string-character } " { LITERAL }
	| '+' { PLUS }
	| '-' { MINUS }
	| '*' { TIMES }
	| '/' { DIVIDE }
	| '=' { EQ }
	| "==" { CMP }
	| eof { EOF }
	| "/*" { comment lexbuf }
and comment =
	parse "*/" { token lexbuf }
	| _ { comment lexbuf }
