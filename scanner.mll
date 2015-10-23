{ open Parser }

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                 { comment lexbuf }
| '('       { LPAREN }    | ')'       { RPAREN }
| '{'       { LBRACE }    | '}'       { RBRACE } 
| ';'       { SEMI }      | ','       { COMMA }
| '+'       { PLUS }      | '-'       { MINUS }
| '*'       { TIMES }     | '/'       { DIVIDE }
| '='       { ASSIGN }     
| "=="      { EQ }        | "!="      { NEQ }
| '<'       { LT }        | '>'       { GT }
| "<="      { LEQ }       | ">="      { GEQ }
| "if"      { IF }        | "else"    { ELSE }
| "while"   { WHILE }     | "for"     { FOR }
| "return"  { RET }       | "STOP"    { STOP }
| '!'       { EXC }       | eof       { EOF }
| ['0'-'9']+ as lxm { INTLITERAL(int_of_string lxm) }
| " { string-character } "+ as lxm { STRLITERAL(lxm) }
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as lxm { VAR(lxm) }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
| "*/"  { token lexbuf }
| _     { comment lexbuf }
