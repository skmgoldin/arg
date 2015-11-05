{ open Parser }

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                 { comment lexbuf }

| "PRINT"   { PRINT }
| '('       { LPAREN }    | ')'       { RPAREN }
| ';'       { SEMI }

| '='       { ASSIGN }     

| eof       { EOF }

| ['0'-'9']+                as lxm { INTLITERAL(int_of_string lxm) }
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as lxm { VAR(lxm) }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
| "*/"  { token lexbuf }
| _     { comment lexbuf }
