{ open Parser }

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                 { comment lexbuf }
| '('       { LPAREN }
| ')'       { RPAREN }
| ','       { COMMA }
| ';'       { SEMI }
| '='       { ASSIGN }
| '{'       { LBRACE }
| '}'       { RBRACE }
| '['       { LBRACK }
| ']'       { RBRACK }
| '+'       { ADD }
| '-'       { SUB }
| '*'       { MULT }
| '/'       { DIV }
| "=="      { EQUAL }
| "!="      { NEQ }
| "<"       { LESS }
| "<="      { LEQ }
| ">"       { GREATER }
| ">="      { GEQ }
| "IF"      { IF }
| "ELSE"    { ELSE }
| "WHILE"   { WHILE }
| "FUNCTION"{ FUNCTION }
| "RETURN"  { RETURN }
| "PRINT"   { PRINT }
| '"'[^'\n''"']*'"' as lxm { STRLITERAL(lxm) }
| ['0'-'9']+     as lxm { INTLITERAL(int_of_string lxm) }
| ['0'-'9']+'.'['0'-'9']    as lxm { FLOATLITERAL(float_of_string lxm)}
| "true"    { BOOLLITERAL(true) }
| "false"   { BOOLLITERAL(false) }
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof       { EOF }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
| "*/"  { token lexbuf }
| _     { comment lexbuf }
