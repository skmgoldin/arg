{ open Parser }

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                 { comment lexbuf }

| '{'       { LBRACE }    | '}'       { RBRACE } 
| ';'       { SEMI }      | ','       { COMMA }

| '('       { LPAREN }    | ')'       { RPAREN }

| '+'       { PLUS }      | '-'       { MINUS }
| '*'       { TIMES }     | '/'       { DIVIDE }
| '='       { ASSIGN }     

| "=="      { EQ }        | "!="      { NEQ }
| '<'       { LT }        | '>'       { GT }
| "<="      { LEQ }       | ">="      { GEQ }

| "int"     { CINT }      | "char"    { CCHAR }
| "double"  { CDOUBLE }   | "bool"    { CBOOL }
| "sizeof"  { SOF }       | "typeof"  { TOF }
| '!'       { EXC }

| '['       { LBRAC }     | ']'       { RBRAC }

| "if"      { IF }        | "else"    { ELSE }
| "while"   { WHILE }     | "return"  { RET }
| "STOP"    { STOP }      | "function"{ FUNC }

| eof       { EOF }

| ['0'-'9']+                as lxm { INTLITERAL(int_of_string lxm) }
| " { string-character } "+ as lxm { STRLITERAL(lxm) }
| (('-'|'+')?['0'-'9']*)?['0'-'9']+ as lxm { FLOATLITERAL(float_of_string lxm) }
| "true"|"false"            as lxm { BOOLLITERAL(bool_of_string lxm) }
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as lxm { VAR(lxm) }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
| "*/"  { token lexbuf }
| _     { comment lexbuf }
