{ open Parser }

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                 { comment lexbuf }

| '"'       { DQUOTE }
| ';'       { SEMI }
| '='       { ASSIGN }
| ','       { COMMA }

| eof       { EOF }

| " { string-character } " as lxm    { STRLITERAL(lxm) }
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
| "*/"  { token lexbuf }
| _     { comment lexbuf }
