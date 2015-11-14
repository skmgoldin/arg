{ open Parser }

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                 { comment lexbuf }
| '('       { LPAREN }
| ')'       { RPAREN }
| ','       { COMMA }
| ';'       { SEMI }
| '='       { ASSIGN }
| '"'[^'\n']*'"' as lxm { STRLITERAL(lxm) }
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof       { EOF }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
| "*/"  { token lexbuf }
| _     { comment lexbuf }
